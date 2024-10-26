/**
  * LAZINESS
  * 
  * Laziness is the most important, as well as most confusing, aspect of using 
  * any functional effect system. 
  * 
  * In this section, you will contrast the "direct" way of encoding computation
  * versus the "lazy" way of encoding computation, and explore some of the 
  * powerful capabilities unlocked by lazy computation.
  */
package net.degoes.laziness

import zio.test._
import zio.test.TestAspect.ignore 

object Job:
  protected val executor = java.util.concurrent.Executors.newFixedThreadPool(4)

  def apply(step: => Unit): Job = ???

trait Job:
  def run(): Unit

  def andThen(job: Job): Job = ???

  def andConcurrently(job: Job): Job = ???

  def eventually: Job = ???

object JobSpec extends ZIOSpecDefault: 
  def spec = 
    suite("JobSpec")(
      test("execution") {
        val job = Job(println("Hello World"))


        job.run()

        /**
         * EXERCISE 1
         * 
         * Implement the primary constructor for `Job` so that when the instance's 
         * `run` method is invoked, the specified by-name step is evaluated.
         */
        assertCompletes
      } @@ ignore,
      test("deferment") {
        var i = 0 

        val job = Job { i += 1 }

        /**
          * EXERCISE 2
          * 
          * This test is currently failing. Explain why the test must 
          * necessarily fail, and then fix the broken assumption so the test 
          * will pass.
          */
        assertTrue(i == 1)
      } @@ ignore,
      test("re-evaluation") {
        var i = 0 

        val job = Job { i += 1 }

        job.run()
        job.run()

        /**
          * EXERCISE 3
          * 
          * This test is currently failing. Explain why the test must 
          * necessarily fail, and then fix the broken assumption so the test 
          * will pass.
          */
        assertTrue(i == 1)
      } @@ ignore,
      test("sequential composition") {
        var i = 0 

        val increment = Job { i += 1 }
        val decrement = Job { i -= 1 }

        val all = increment.andThen(decrement).andThen(increment)

        all.run()

        /**
          * EXERCISE 4
          * 
          * This test is failing because the `andThen` operator has not been 
          * implemented. Implement the operator in such a way as to make the 
          * test pass and try to reverse engineer its meaning.
          */
        assertTrue(i == 1)
      } @@ ignore,
      test("parallel composition") {
        @volatile var first = 0 
        @volatile var second = 0 

        val waitUntilFirst  = Job { second += 1; while (first != 1) Thread.`yield`() }
        val waitUntilSecond = Job { first += 1; while (second != 1) Thread.`yield`() }
        

        val both = waitUntilFirst.andConcurrently(waitUntilSecond)

        both.run()
        
        /**
          * EXERCISE 5
          * 
          * Implement the `andConcurrently` method so that the job it returns 
          * will run both of the original jobs concurrently, on separate 
          * threads in a thread pool.
          */
        assertCompletes
      } @@ ignore,
      test("error recovery") {
        val flakyJob = Job { if (Math.random() < 0.9) throw new Error("Uh oh!") else () }

        flakyJob.eventually.run() 

        /**
          * EXERCISE 6
          * 
          * Implement the `eventually` operator so that the job it returns will 
          * continuously retry until the original job does not fail (by throwing
          * an exception).
          */
        assertCompletes
      }
    )

object Workflow:
  protected val executor = java.util.concurrent.Executors.newFixedThreadPool(4)

  def apply[A](step: => A): Workflow[A] = ???

trait Workflow[+A]:
  def run(): A

  def andThen[B](next: Workflow[B]): Workflow[(A, B)] = ???

  def andConcurrently[B](concurrent: Workflow[B]): Workflow[(A, B)] = ???

  def eventually: Workflow[A] = ???

object WorkflowSpec extends ZIOSpecDefault: 
  def spec = 
    suite("WorkflowSpec")(
      test("execution") {
        val workflow = Workflow(42)

        /**
         * EXERCISE 7
         * 
         * Implement the primary constructor for `Workflow` so that when the 
         * instance's `run` method is invoked, the specified by-name step is 
         * evaluated.
         */
        assertTrue(workflow.run() == 42)
      } @@ ignore,
      test("sequential composition") {
        var i = 0 

        val increment = Workflow { i += 1 }
        val decrement = Workflow { i -= 1 }

        val all = increment.andThen(decrement).andThen(increment)

        all.run()

        /**
          * EXERCISE 8
          * 
          * This test is failing because the `andThen` operator has not been 
          * implemented. Implement the operator in such a way as to make the 
          * test pass and try to reverse engineer its meaning.
          */
        assertTrue(i == 1)
      } @@ ignore,
      test("parallel composition") {
        @volatile var first = 0 
        @volatile var second = 0 

        val waitUntilFirst  = Workflow { second += 1; while (first != 1) Thread.`yield`(); first }
        val waitUntilSecond = Workflow { first += 1; while (second != 1) Thread.`yield`(); second }        

        val both = waitUntilFirst.andConcurrently(waitUntilSecond)

        /**
          * EXERCISE 9
          * 
          * Implement the `andConcurrently` method so that the workflow it
          * returns will run both of the original workflows concurrently, on
          * separate threads in a thread pool.
          */
        assertTrue(both.run() == (1, 1))
      } @@ ignore,
      test("error recovery") {
        val flakyWorkflow = Workflow { if (Math.random() < 0.9) throw new Error("Uh oh!") else 42 }

        /**
          * EXERCISE 10
          * 
          * Implement the `eventually` operator so that the workflow it returns 
          * will continuously retry until the original workflow does not fail 
          * (by throwing an exception).
          */
        assertTrue(flakyWorkflow.eventually.run() == 42)
      }
    )

object Effect:
  def apply[A](step: => A): Effect[A] = ???

trait Effect[+A]:
  def run(): A

  def map[B](f: A => B): Effect[B] = ???

  def flatMap[B](f: A => Effect[B]): Effect[B] = ???

  def zip[B](that: Effect[B]): Effect[(A, B)] = ???

  def zipPar[B](that: Effect[B]): Effect[(A, B)] = ???

  def eventually: Effect[A] = ???

object EffectSpec extends ZIOSpecDefault:
  def spec = 
    suite("EffectSpec")(
      test("execution") {
        val effect = Effect(42)

        /**
          * EXERCISE 11
          * 
          * Implement the primary constructor for `Effect` so that when the
          * instance's `run` method is invoked, the specified by-name step is
          * evaluated.
          */
        assertTrue(effect.run() == 42)
      } @@ ignore,
      test("sequential composition") {
        var i = 0 

        val increment = Effect { i += 1 }
        val decrement = Effect { i -= 1 }

        val all = increment.flatMap(_ => decrement).flatMap(_ => increment)

        all.run()

        /**
          * EXERCISE 12
          * 
          * This test is failing because the `flatMap` operator has not been
          * implemented. Implement the operator in such a way as to make the
          * test pass and try to reverse engineer its meaning.
          */
        assertTrue(i == 1)
      } @@ ignore,
      test("parallel composition") {
        @volatile var first = 0 
        @volatile var second = 0 

        val waitUntilFirst  = Effect { second += 1; while (first != 1) Thread.`yield`(); first }
        val waitUntilSecond = Effect { first += 1; while (second != 1) Thread.`yield`(); second }        

        val both = waitUntilFirst.zipPar(waitUntilSecond)

        /**
          * EXERCISE 13
          * 
          * Implement the `zipPar` method so that the effect it returns will run
          * both of the original effects concurrently, on separate threads in a
          * thread pool.
          */
        assertTrue(both.run() == (1, 1))
      } @@ ignore,
      test("error recovery") {
        val flakyEffect = Effect { if (Math.random() < 0.9) throw new Error("Uh oh!") else 42 }

        assertTrue(flakyEffect.eventually.run() == 42)
      }
    )