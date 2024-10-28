/** CONCURRENCY
  *
  * Achieving scalable concurrency is a critical feature of any functional effect system. However, functional effect
  * systems tend to go much further than just making concurrency scalable--they offer safer and more compositional
  * concurrency primitives.
  *
  * In this section, you will explore some of the foundational elements of concurrency in effect system, learning by
  * implementing. What you learn will improve your understanding of concurrency, as well as apply to any functional
  * effect system.
  */
package net.degoes.concurrency

import zio.test._
import zio.test.TestAspect.ignore

import java.util.concurrent.atomic.AtomicReference

import net.degoes.asynchronicity.Async

object Signal:
  def make: Async[Signal] = ???

class Signal(private val atomic: AtomicReference[Option[List[(unit: Unit) => Unit]]]):
  def await: Async[Unit] = ???

  def isTriggered: Async[Boolean] = ???

  def trigger: Async[Unit] = ???

object SignalSpec extends ZIOSpecDefault:
  def spec =
    suite("SignalSpec")(
      test("make") {
        val signal = Signal.make.block

        /** EXERCISE 1
          *
          * Implement the `make` method on `Signal` so that it returns an `Async` that produces a new `Signal`.
          */
        assertTrue(signal != null)
      } @@ ignore,
      test("await + trigger") {
        val signal = Signal.make.block

        val runnable: Runnable = () => signal.trigger.block

        Async.scheduledExecutor.schedule(runnable, 10, java.util.concurrent.TimeUnit.MILLISECONDS)

        /** EXERCISE 2
          *
          * Implement the `await` and `trigger` methods on `Signal` so that the `await` method returns an `Async` that
          * blocks until the signal is triggered, and the `trigger` method returns an `Async` that triggers the signal.
          */
        assertTrue(signal.await.block == ())
      } @@ ignore,
      test("isTriggered") {
        val signal = Signal.make.block

        val runnable: Runnable = () => signal.trigger.block

        Async.scheduledExecutor.schedule(runnable, 10, java.util.concurrent.TimeUnit.MILLISECONDS)

        /** EXERCISE 3
          *
          * Implement the `isTriggered` method on `Signal` so that it returns an `Async` that immediately indicates
          * whether the signal has been triggered.
          */
        assertTrue(signal.isTriggered.block)
      } @@ ignore,
    )

object Ref:
  def make[A](a: A): Async[Ref[A]] = ???

class Ref[A](private val atomic: AtomicReference[A]):
  def get: Async[A] = ???

  def set(a: A): Async[Unit] = ???

  def update(f: A => A): Async[Unit] = ???

object RefSpec extends ZIOSpecDefault:
  def spec =
    suite("RefSpec")(
      test("make") {
        val ref = Ref.make(42).block

        /** EXERCISE 4
          *
          * Implement the `make` method on `Ref` so that it returns an `Async` that produces a new `Ref` with the
          * specified initial value.
          */
        assertTrue(ref != null)
      } @@ ignore,
      test("get + set") {
        val ref = Ref.make(42).block

        ref.set(43).block

        /** EXERCISE 5
          *
          * Implement the `get` and `set` methods on `Ref` so that `get` returns an `Async` that gets the current value
          * of the `Ref`, and `set` returns an `Async` that sets the value of the `Ref`.
          */
        assertTrue(ref.get.block == 43)
      } @@ ignore,
      test("update") {
        val ref = Ref.make(42).block

        ref.update(_ + 1).block

        /** EXERCISE 6
          *
          * Implement the `update` method on `Ref` so that it returns an `Async` that updates the value of the `Ref`
          * using the specified function.
          */
        assertTrue(ref.get.block == 43)
      } @@ ignore,
    )

class GreenThread[A] private ():
  def join: Async[A] = ???

extension [A](async: Async[A]) def fork: Async[GreenThread[A]] = ???

object GreenThreadSpec extends ZIOSpecDefault:
  def spec =
    suite("GreenThreadSpec")(
      test("fork + join") {
        def par[A, B](a: Async[A], b: Async[B]): Async[(A, B)] =
          for {
            ga <- a.fork
            gb <- b.fork
            a  <- ga.join
            b  <- gb.join
          } yield (a, b)

        val a = Async.succeed(42)
        val b = Async.succeed("Hello")

        val result = par(a, b).block

        /** EXERCISE 7
          *
          * Implement the `fork` and `join` methods on `Async` so that the following test passes.
          */
        assertTrue(result == (42, "Hello"))
      } @@ ignore
    )

object Promise:
  def make[A]: Async[Promise[A]] = ???

class Promise[A] private (signal: Signal, ref: Ref[Option[A]]):
  def await: Async[A] = ???

  def complete(a: A): Async[Unit] = ???

object PromiseSpec extends ZIOSpecDefault:
  def spec =
    suite("PromiseSpec")(
      test("make") {
        val promise = Promise.make[Int].block

        /** EXERCISE 8
          *
          * Implement the `make` method on `Promise` so that it returns an `Async` that produces a new `Promise`.
          */
        assertTrue(promise != null)
      } @@ ignore,
      test("await + complete") {
        val promise = Promise.make[Int].block

        /** EXERCISE 9
          *
          * Implement the `await` and `complete` methods on `Promise` so that the `await` method returns an `Async` that
          * blocks until the promise is completed, and the `complete` method returns an `Async` that completes the
          * promise.
          */
        assertTrue(promise.complete(42).fork.flatMap(_ => promise.await).block == 42)
      } @@ ignore,
    )

object Queue:
  def make[A]: Async[Queue[A]] = ???

class Queue[A] private ():
  def offer(a: A): Async[Unit] = ???

  def take: Async[A] = ???

object QueueSpec extends ZIOSpecDefault:
  def spec =
    suite("QueueSpec")(
      test("make") {
        val queue = Queue.make[Int].block

        /** EXERCISE 10
          *
          * Implement the `make` method on `Queue` so that it returns an `Async` that produces a new `Queue`.
          */
        assertTrue(queue != null)
      } @@ ignore,
      test("offer + take") {
        val queue = Queue.make[Int].block

        /** EXERCISE 11
          *
          * Implement the `offer` and `take` methods on `Queue` so that the `offer` method returns an `Async` that adds
          * an element to the queue, and the `take` method returns an `Async` that removes an element from the queue.
          */
        assertTrue(queue.offer(42).fork.flatMap(_ => queue.take).block == 42)
      } @@ ignore,
    )

object Semaphore:
  def make(n: Int): Async[Semaphore] = ???

class Semaphore private (val capacity: Int):
  def acquire: Async[Unit] = ???

  def release: Async[Unit] = ???

object SemaphoreSpec extends ZIOSpecDefault:
  def spec =
    suite("SemaphoreSpec")(
      test("make") {
        val semaphore = Semaphore.make(1).block

        /** EXERCISE 12
          *
          * Implement the `make` method on `Semaphore` so that it returns an `Async` that produces a new `Semaphore`
          * with the specified capacity.
          */
        assertTrue(semaphore != null)
      } @@ ignore,
      test("acquire + release") {
        val semaphore = Semaphore.make(1).block

        /** EXERCISE 13
          *
          * Implement the `acquire` and `release` methods on `Semaphore` so that the `acquire` method returns an `Async`
          * that blocks until the semaphore is available, and the `release` method returns an `Async` that releases the
          * semaphore.
          */
        assertTrue(semaphore.acquire.fork.flatMap(_ => semaphore.release).block == ())
      } @@ ignore,
      test("concurrent acquire") {
        val semaphore = Semaphore.make(1).block

        val test =
          for {
            thread1 <- semaphore.acquire.flatMap(_ => semaphore.release).fork
            thread2 <- semaphore.acquire.flatMap(_ => semaphore.release).fork
            _       <- thread1.join
            _       <- thread2.join
          } yield 42

        /** EXERCISE 14
          *
          * Implement the `acquire` method on `Semaphore` so that it can be acquired concurrently by multiple fibers.
          */
        assertTrue(test.block == 42)
      } @@ ignore,
    )
