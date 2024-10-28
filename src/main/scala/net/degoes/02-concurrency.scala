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
  val make: Async[Signal] = 
    Async.succeed(new Signal(new AtomicReference(Some(Nil))))

class Signal(private val atomic: AtomicReference[Option[List[(unit: Unit) => Unit]]]):
  def await: Async[Unit] = 
    Async[Unit](callback =>
      var callNow = false

      atomic.updateAndGet:
        case None => callNow = true; None
        case Some(callbacks) => callNow = false; Some(callback :: callbacks)

      if callNow then callback(())
    )

  def isTriggered: Async[Boolean] = 
    Async.succeed(atomic.get().isEmpty)

  def trigger: Async[Unit] = 
    Async[Unit](callback =>
      var callbacks: List[Unit => Unit] = Nil 

      atomic.updateAndGet:
        case None => callbacks = Nil; None 
        case Some(callbacks0) => callbacks = callbacks0; None

      callbacks.foreach(callback => callback(()))

      callback(())
    )

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
      },
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
      },
      test("isTriggered") {
        val signal = Signal.make.block

        signal.trigger.block

        /** EXERCISE 3
          *
          * Implement the `isTriggered` method on `Signal` so that it returns an `Async` that immediately indicates
          * whether the signal has been triggered.
          */
        assertTrue(signal.isTriggered.block)
      },
    )

object Ref:
  def make[A](a: A): Async[Ref[A]] = 
    Async.succeed(Ref(new AtomicReference(a)))

class Ref[A](private val atomic: AtomicReference[A]):
  def get: Async[A] = 
    Async.succeed(atomic.get())

  def set(a: A): Async[Unit] = 
    Async.succeed(atomic.set(a))

  def update(f: A => A): Async[Unit] = 
    Async.succeed:
      atomic.updateAndGet(a => f(a))

      ()

  def modify[B](f: A => (B, A)): Async[B] = 
    Async.succeed:
      var option = Option.empty[B]

      atomic.updateAndGet: a0 => 
        option = Option.empty[B]

        val (b, a) = f(a0)
        
        option = Some(b)

        a

      option.get

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
      },
      test("get + set") {
        val ref = Ref.make(42).block

        ref.set(43).block

        /** EXERCISE 5
          *
          * Implement the `get` and `set` methods on `Ref` so that `get` returns an `Async` that gets the current value
          * of the `Ref`, and `set` returns an `Async` that sets the value of the `Ref`.
          */
        assertTrue(ref.get.block == 43)
      },
      test("update") {
        val ref = Ref.make(42).block

        ref.update(_ + 1).block

        /** EXERCISE 6
          *
          * Implement the `update` method on `Ref` so that it returns an `Async` that updates the value of the `Ref`
          * using the specified function.
          */
        assertTrue(ref.get.block == 43)
      },
    )

final case class GreenThread[A](doneSignal: Signal, value: Ref[Option[A]]):
  def join: Async[A] = 
    for 
      _ <- doneSignal.await 
      option <- value.get 
    yield option.get

extension [A](async: Async[A]) def fork: Async[GreenThread[A]] = 
  for 
    signal <- Signal.make 
    ref    <- Ref.make(Option.empty[A])
  yield 
    (for 
      a <- async
      _ <- ref.set(Some(a))
      _ <- signal.trigger
    yield ()).subscribe(_ => ())

    GreenThread(signal, ref)


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
      }
    )

object Promise:
  def make[A]: Async[Promise[A]] = 
    for 
      signal <- Signal.make 
      ref <- Ref.make[Option[A]](None)
    yield Promise(signal, ref)

class Promise[A] private (signal: Signal, ref: Ref[Option[A]]):
  def await: Async[A] = 
    for 
      _ <- signal.await 
      option <- ref.get 
    yield option.get   

  def complete(a: A): Async[Unit] = 
    for
      _ <- ref.update:
        case x @ Some(_) => x 
        case None => Some(a)
      _ <- signal.trigger
    yield ()

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
      },
      test("await + complete") {
        val promise = Promise.make[Int].block

        /** EXERCISE 9
          *
          * Implement the `await` and `complete` methods on `Promise` so that the `await` method returns an `Async` that
          * blocks until the promise is completed, and the `complete` method returns an `Async` that completes the
          * promise.
          */
        assertTrue(promise.complete(42).fork.flatMap(_ => promise.await).block == 42)
      },
    )

object Queue:
  def make[A]: Async[Queue[A]] = ???

class Queue[A](ref: Ref[Either[::[A], List[Promise[A]]]]):
  def offer(a: A): Async[Unit] = 
    def complete(option: Option[Promise[A]]): Async[Unit] = 
      option match 
        case None => Async.succeed(())
        case Some(promise) => promise.complete(a)

    val modifyState: Async[Option[Promise[A]]] = 
      ref.modify[Option[Promise[A]]]:
        case Left(as)       => (None, Left(::(a, as)))
        case Right(p :: ps) => (Some(p), Right(ps))
        case Right(Nil)     => (None, Left(::(a, Nil)))
    
    for 
      option <- modifyState 
      _      <- complete(option)
    yield ()

  def take: Async[A] = 
    def modifyState(promise: Promise[A]): Async[Option[A]] = 
      ref.modify[Option[A]] {
        case Left(::(a, as)) => 
          val newState =
            as match
              case x @ (_ :: _) => Left(x)
              case Nil => Right(Nil)

          (Some(a), newState)
          
        case Right(promises) => 
          val newState = 
            Right(promise :: promises)

          (None, newState)
      }

    def completePromise(option: Option[A], promise: Promise[A]): Async[Unit] = 
      option match 
        case None => Async.succeed(())
        case Some(a) => promise.complete(a)

    for 
      promise <- Promise.make[A]
      option  <- modifyState(promise)
      _       <- completePromise(option, promise)
      a       <- promise.await
    yield a

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
