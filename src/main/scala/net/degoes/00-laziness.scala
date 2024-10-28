/** LAZINESS
  *
  * Laziness is the most important, as well as most confusing, aspect of using any functional effect system.
  *
  * In this section, you will contrast the "direct" way of encoding computation versus the "lazy" way of encoding
  * computation, and explore some of the powerful capabilities unlocked by lazy computation.
  */
package net.degoes.laziness

import zio.test._
import zio.test.TestAspect.ignore

object Job:
  protected val executor = java.util.concurrent.Executors.newFixedThreadPool(4)

  def apply(step: => Unit): Job =
    new Job:
      def run(): Unit = step

trait Job:
  self => 
    def run(): Unit

    def andThen(job: Job): Job = 
      new Job:
        def run(): Unit = 
          self.run()
          job.run()

    def andConcurrently(job: Job): Job =
      new Job:
        def run(): Unit = 
          val left = 
            new Thread:
              override def run(): Unit = self.run()

          val right = 
            new Thread:
              override def run(): Unit = job.run()

          left.start()
          right.start()

          left.join()
          right.join()

    def eventually: Job = 
      new Job:
        def run(): Unit = 
          var trying = true 

          while (trying)
            try
              self.run() 
              trying = false
            catch 
              case _ : Throwable => 
            

object JobSpec extends ZIOSpecDefault:
  def spec =
    suite("JobSpec")(
      test("execution") {
        val job = Job(println("Hello World"))

        job.run()

        /** EXERCISE 1
          *
          * Implement the primary constructor for `Job` so that when the instance's `run` method is invoked, the
          * specified by-name step is evaluated.
          */
        assertCompletes
      },
      test("deferment") {
        var i = 0

        val job = Job(i += 1)

        job.run()

        /** EXERCISE 2
          *
          * This test is currently failing. Explain why the test must necessarily fail, and then fix the broken
          * assumption so the test will pass.
          */
        assertTrue(i == 1)
      },
      test("re-evaluation") {
        var i = 0

        val job = Job(i += 1)

        job.run()
        job.run()      

        /** EXERCISE 3
          *
          * This test is currently failing. Explain why the test must necessarily fail, and then fix the broken
          * assumption so the test will pass.
          */
        assertTrue(i == 2)
      },
      test("sequential composition") {
        var i = 0

        val increment = Job(i += 1)
        val decrement = Job(i -= 1)

        val all = increment.andThen(decrement).andThen(increment)

        all.run()

        /** EXERCISE 4
          *
          * This test is failing because the `andThen` operator has not been implemented. Implement the operator in such
          * a way as to make the test pass and try to reverse engineer its meaning.
          */
        assertTrue(i == 1)
      },
      test("parallel composition") {
        @volatile var first  = 0
        @volatile var second = 0

        val waitUntilFirst  = Job { second += 1; while (first != 1) Thread.`yield`() }
        val waitUntilSecond = Job { first += 1; while (second != 1) Thread.`yield`() }

        val both = waitUntilFirst.andConcurrently(waitUntilSecond)

        both.run()

        /** EXERCISE 5
          *
          * Implement the `andConcurrently` method so that the job it returns will run both of the original jobs
          * concurrently, on separate threads in a thread pool.
          */
        assertCompletes
      } @@ ignore,
      test("error recovery") {
        val flakyJob = Job(if (Math.random() < 0.9) throw new Error("Uh oh!") else ())

        val nonFlakyJob = flakyJob.eventually
        
        nonFlakyJob.run()

        /** EXERCISE 6
          *
          * Implement the `eventually` operator so that the job it returns will continuously retry until the original
          * job does not fail (by throwing an exception).
          */
        assertCompletes
      },
    )

object Workflow:
  protected val executor = java.util.concurrent.Executors.newFixedThreadPool(4)

  def apply[A](step: => A): Workflow[A] = 
    new Workflow:
      def run(): A = step

trait Workflow[+A]:
  self => 
    def run(): A

    def andThen[B](next: Workflow[B]): Workflow[(A, B)] = 
      new Workflow[(A, B)]:
        def run(): (A, B) = 
          val a: A = self.run() 
          val b: B = next.run() 

          (a, b)

    def andConcurrently[B](concurrent: Workflow[B]): Workflow[(A, B)] = 
      new Workflow[(A, B)]:
        def run(): (A, B) = 
          var leftValue = Option.empty[A]
          var rightValue = Option.empty[B]

          val left = 
            new Thread:
              override def run(): Unit = 
                leftValue = Some(self.run())

          val right = 
            new Thread:
              override def run(): Unit = 
                rightValue = Some(concurrent.run())

          left.start()
          right.start()

          left.join()
          right.join()

          (leftValue.get, rightValue.get)

    def eventually: Workflow[A] = 
      new Workflow[A]:
        def run(): A = 
          var value = Option.empty[A]

          while (value.isEmpty)
            try
              value = Some(self.run())
            catch 
              case _ : Throwable => 

          value.get

object WorkflowSpec extends ZIOSpecDefault:
  def spec =
    suite("WorkflowSpec")(
      test("execution") {
        val workflow = Workflow(42)

        /** EXERCISE 7
          *
          * Implement the primary constructor for `Workflow` so that when the instance's `run` method is invoked, the
          * specified by-name step is evaluated.
          */
        assertTrue(workflow.run() == 42)
      },
      test("sequential composition") {
        var i = 0

        val increment = Workflow(i += 1)
        val decrement = Workflow(i -= 1)

        val all = increment.andThen(decrement).andThen(increment)

        all.run()

        /** EXERCISE 8
          *
          * This test is failing because the `andThen` operator has not been implemented. Implement the operator in such
          * a way as to make the test pass and try to reverse engineer its meaning.
          */
        assertTrue(i == 1)
      } @@ ignore,
      test("parallel composition") {
        @volatile var first  = 0
        @volatile var second = 0

        val waitUntilFirst  = Workflow { second += 1; while (first != 1) Thread.`yield`(); first }
        val waitUntilSecond = Workflow { first += 1; while (second != 1) Thread.`yield`(); second }

        val both = waitUntilFirst.andConcurrently(waitUntilSecond)

        /** EXERCISE 9
          *
          * Implement the `andConcurrently` method so that the workflow it returns will run both of the original
          * workflows concurrently, on separate threads in a thread pool.
          */
        assertTrue(both.run() == (1, 1))
      } @@ ignore,
      test("error recovery") {
        val flakyWorkflow = Workflow(if (Math.random() < 0.9) throw new Error("Uh oh!") else 42)

        /** EXERCISE 10
          *
          * Implement the `eventually` operator so that the workflow it returns will continuously retry until the
          * original workflow does not fail (by throwing an exception).
          */
        assertTrue(flakyWorkflow.eventually.run() == 42)
      },
    )

object Effect:
  def apply[A](step: => A): Effect[A] = 
    new Effect:
      def run(): A = step

trait Effect[+A]:
  self => 
    def run(): A

    def map[B](f: A => B): Effect[B] = 
      new Effect:
        def run(): B = 
          f(self.run())

    def flatMap[B](f: A => Effect[B]): Effect[B] = 
      new Effect:
        def run(): B = 
          val a = self.run()

          f(a).run()

    def zip[B](that: Effect[B]): Effect[(A, B)] = 
      flatMap(a => that.map(b => (a, b)))

    def zipPar[B](that: Effect[B]): Effect[(A, B)] = 
      new Effect[(A, B)]:
        def run(): (A, B) = 
          var leftValue = Option.empty[A]
          var rightValue = Option.empty[B]

          val left = 
            new Thread:
              override def run(): Unit = 
                leftValue = Some(self.run())

          val right = 
            new Thread:
              override def run(): Unit = 
                rightValue = Some(that.run())

          left.start()
          right.start()

          left.join()
          right.join()

          (leftValue.get, rightValue.get)

    def eventually: Effect[A] = 
      new Effect[A]:
        def run(): A = 
          var value = Option.empty[A]

          while (value.isEmpty)
            try
              value = Some(self.run())
            catch 
              case _ : Throwable => 

          value.get

object EffectSpec extends ZIOSpecDefault:
  def spec =
    suite("EffectSpec")(
      test("execution") {
        val effect = Effect(42).map(int => int.toString())

        /** EXERCISE 11
          *
          * Implement the primary constructor for `Effect` so that when the instance's `run` method is invoked, the
          * specified by-name step is evaluated.
          */
        assertTrue(effect.run() == "42")
      },
      test("sequential composition") {
        var i = 0

        val increment = Effect(i += 1)
        val decrement = Effect(i -= 1)

        // increment.flatMap(a => decrement.flatMap(b => increment.map(c =>())))
        val all = 
          for 
            _ <- increment
            _ <- decrement
            _ <- increment
          yield ()

        all.run()

        /** EXERCISE 12
          *
          * This test is failing because the `flatMap` operator has not been implemented. Implement the operator in such
          * a way as to make the test pass and try to reverse engineer its meaning.
          */
        assertTrue(i == 1)
      } @@ ignore,
      test("parallel composition") {
        @volatile var first  = 0
        @volatile var second = 0

        val waitUntilFirst  = Effect { second += 1; while (first != 1) Thread.`yield`(); first }
        val waitUntilSecond = Effect { first += 1; while (second != 1) Thread.`yield`(); second }

        val both = waitUntilFirst.zipPar(waitUntilSecond)

        /** EXERCISE 13
          *
          * Implement the `zipPar` method so that the effect it returns will run both of the original effects
          * concurrently, on separate threads in a thread pool.
          */
        assertTrue(both.run() == (1, 1))
      } @@ ignore,
      test("error recovery") {
        val flakyEffect = Effect(if (Math.random() < 0.9) throw new Error("Uh oh!") else 42)

        assertTrue(flakyEffect.eventually.run() == 42)
      },
    )

trait EffectMain:
  def main(args: Array[String]): Unit = effect.run()

  def effect: Effect[Any]

object Demo extends EffectMain:
  def printLine(line: String): Effect[Unit] = 
    Effect(println(line))

  val readLine: Effect[String] = 
    Effect(scala.io.StdIn.readLine())

  def convertToDogYears(string: String): Int = 
    string.toInt / 7

  val effect = 
    for 
      _   <- printLine("What is your age in human years?")
      age <- readLine
      _   <- printLine(s"Your age in dog years is ${convertToDogYears(age)}")
    yield ()



  