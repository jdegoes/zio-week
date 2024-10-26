/**
  * ASYNCHRONICITY
  * 
  * One of the compelling benefits of a functional effect system is that they 
  * provide _scalable concurrency_, thanks to their implementation through 
  * asynchronous (callback-based) programming.
  * 
  * In this section, you will explore both the motivation for asynchronicity,
  * as well as the details of how functional effect systems implement 
  * lightweight concurrency.
  */
package net.degoes.asynchronicity

import zio.test._
import zio.test.TestAspect.ignore 
import java.util.concurrent.ScheduledExecutorService

def time(label: String)(f: => Unit): Unit = 
  val start = System.currentTimeMillis()
  f
  val end = System.currentTimeMillis()
  println(s"$label: ${end - start}ms")

object OSThreadTester:
  def startThread(): Unit = (new Thread:
    override def run(): Unit = Thread.sleep(10)).start()

  @main
  def startAMillion1(): Unit = 
    /**
      * EXERCISE 1
      * 
      * Using the `time` function, measure how long it takes to start a million
      * OS threads. 
      */
    time("A million OS threads"):
      ???

object GreenThreadTester:
    val scheduler = java.util.concurrent.Executors.newScheduledThreadPool(4)

    class GreenThread extends Runnable:
      def run(): Unit = Thread.sleep(10)

    def startGreenThread(): Unit = scheduler.schedule(new GreenThread, 10, java.util.concurrent.TimeUnit.MILLISECONDS)


    /**
     * EXERCISE 2
     * 
     * Using the `time` function, measure how long it takes to start a million
     * green threads. Compare the difference in performance between OS threads
     * and green threads and comment on the results.
     */
    @main
    def startAMillion2(): Unit = 
      time("A million green threads"):
        ???


trait Async[+A]:
  def subscribe(callback: A => Unit): Unit

  def map[B](f: A => B): Async[B] = ???

  def sequence[B](that: Async[B]): Async[(A, B)] = ???

  def flatMap[B](f: A => Async[B]): Async[B] = ???

  def zipPar[B](that: Async[B]): Async[(A, B)] = ???

  // This is intentionally broken. You will fix it in the exercises.
  def block: A = 
    var result: Option[A] = None
    
    subscribe { a => 
      result = Some(a)
    }

    result.get

object Async:
  val scheduledExecutor: ScheduledExecutorService = java.util.concurrent.Executors.newScheduledThreadPool(4)

  def apply[A](f: (A => Unit) => Unit): Async[A] = new Async[A]:
    def subscribe(callback: A => Unit): Unit = f(callback)

  def succeed[A](value: A): Async[A] = ???

  def sleep(duration: Long): Async[Unit] = ???

  def awaitAll[A](asyncs: Iterable[Async[A]]): Async[Seq[A]] = ???

  def awaitAll[A](asyncs: Async[A]*): Async[Seq[A]] = awaitAll(asyncs.toSeq)  

object AsyncSpec extends ZIOSpecDefault:
  def spec = 
    suite("AsyncSpec")(
      test("succeed") {
        val async = Async.succeed(42)

        /**
          * EXERCISE 3
          * 
          * Implement the `success` method on `Async` so that it returns an
          * `Async` that immediately succeeds with the specified value.
          */
        assertTrue(async.block == 42)
      } @@ ignore,
      test("map") {
        val async = Async.succeed(42).map(_ + 1)

        /**
          * EXERCISE 4
          * 
          * Implement the `map` method on `Async` so that it returns an
          * `Async` that applies the specified function to the value produced
          * by the source `Async`.
          */
        assertTrue(async.block == 43)
      } @@ ignore,
      test("sequence") {
        val async = Async.succeed(42).sequence(Async.succeed("Hello"))

        /**
          * EXERCISE 5
          * 
          * Implement the `sequence` method on `Async` so that it returns an
          * `Async` that produces a tuple of the values produced by the source
          * `Async` and the specified `Async`.
          */
        assertTrue(async.block == (42, "Hello"))
      } @@ ignore,
      test("flatMap") {
        val async = Async.succeed(42).flatMap(n => Async.succeed(n + 1))

        /**
          * EXERCISE 6
          * 
          * Implement the `flatMap` method on `Async` so that it returns an
          * `Async` that applies the specified function to the value produced
          * by the source `Async`.
          */
        assertTrue(async.block == 43)
      } @@ ignore,
      test("block") {

        val async = Async { callback => 
          val runnable: Runnable = () => callback(42)

          Async.scheduledExecutor.schedule(runnable, 10, java.util.concurrent.TimeUnit.MILLISECONDS)
        }

        /**
          * EXERCISE 7
          * 
          * Implement the `block` method on `Async` so that it blocks the
          * current thread until the value is produced by the `Async`.
          */
        assertTrue(async.block == 42)
      } @@ ignore,
      test("sleep") {
        val start = System.currentTimeMillis()

        Async.sleep(100).block

        val end = System.currentTimeMillis()

        /**
          * EXERCISE 8
          * 
          * Implement the `sleep` method on `Async` so that it returns an
          * `Async` that sleeps for the specified duration before producing
          * a value.
          */
        assertTrue(end - start >= 100)
      } @@ ignore,
      test("zipPar") {
        val async = Async.sleep(10).map(_  => 42).zipPar(Async.sleep(10).map(_ => "Hello"))

        /**
          * EXERCISE 9
          * 
          * Implement the `zipPar` method on `Async` so that it returns an
          * `Async` that produces a tuple of the values produced by the source
          * `Async` and the specified `Async`, running both computations 
          * concurrently.
          */
        assertTrue(async.block == (42, "Hello"))
      } @@ ignore,
      test("awaitAll") {
        val async = 
          Async.awaitAll(
            Async.sleep(10).map(_  => 42), 
            Async.sleep(10).map(_ => "Hello")
          )

        /**
          * EXERCISE 10
          * 
          * Implement the `awaitAll` method on `Async` so that it returns an
          * `Async` that produces a sequence of the values produced by the
          * specified `Async` values.
          */
        assertTrue(async.block == Seq(42, "Hello"))
      } @@ ignore
    )
