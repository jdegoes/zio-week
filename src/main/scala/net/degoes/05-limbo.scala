/** LIMBO
  *
  * You've learned enough for now. Time to have some fun! Using your own knowledge of functional effects, and whatever
  * you can learn using ZIO docs and tutorials, see how far you can get implementing a series of increasingly complex
  * requirements.
  */
package net.degoes.limbo

import zio._
import zio.test._
import zio.test.TestAspect.ignore

import java.sql.SQLException

final case class Response(code: Int, body: String, headers: Map[String, String])

final case class HttpException(code: Int, message: String) extends Exception(message):
  def toResponse: Response = Response(code, message, Map("Content-Type" -> "text/plain"))

case class InsufficientFunds() extends Exception


object ZIOLimboSpec extends ZIOSpecDefault:
  def spec =
    suite("ZIOLimboSpec")(
      test("unrecoverable") {
        val makeCharge: ZIO[Any, InsufficientFunds, Unit] = ZIO.unit

        val recordInDatabase: ZIO[Any, SQLException, Unit] =
          ZIO.fail(new SQLException("Database is down!"))

        /** EXERCISE 1
          *
          * This effect is reflecting non-recoverable errors in the erorr channel (specifically, SQLException). Use the
          * right operators to adjust the type so that the failure channel only reflects recoverable errors
          * (InsufficientFunds).
          */
        def chargeUser: ZIO[Any, InsufficientFunds | SQLException, Unit] =
          (for
            _ <- makeCharge
            _ <- recordInDatabase
          yield ()).refineOrDie:
            case e : InsufficientFunds => e 

        for result <- chargeUser.sandbox.flip
        yield assertTrue(result.defects.head.isInstanceOf[SQLException])
      } @@ ignore,
      test("looking for trouble") {        
        /** EXERCISE 2
          *
          * Implement the `look` function so that it returns a new effect that that will succeed with a valid response
          * only if the original effect fails with an `HttpException` with a code of 404. Otherwise, the returned effect
          * should fail with `None`.
          */
        def look(effect: ZIO[Any, Throwable, Response]): ZIO[Any, Option[Nothing], Response] =
          (effect.mapError: 
            case e @ HttpException(404, _) => Some(e.toResponse)
            case _ => None).flip.orElse(ZIO.none.flip).some.mapError(_.flatten)

        for
          success <- look(ZIO.fail(HttpException(404, "Not Found")))
          failure <- look(ZIO.succeed(Response(200, "OK", Map.empty))).flip
        yield assertTrue(success.code == 404 && failure == None)
      } @@ ignore,
      test("poor man's zipPar") {

        /** EXERCISE 3
          *
          * Using `ZIO#fork` and `Fiber#join`, implement a function that takes two effects, and returns an effect that
          * concurrently executes both left and right effects, succeeding with a tuple of their results.
          */
        def zipPar[R, E, A](left: ZIO[R, E, A], right: ZIO[R, E, A]): ZIO[R, E, (A, A)] =
          for 
            promise    <- Promise.make[E, A]
            rightFiber <- right.intoPromise(promise).fork 
            leftA      <- left
            rightA     <- promise.await
          yield (leftA, rightA)

        for result <- zipPar(ZIO.succeed(42), ZIO.succeed(42))
        yield assertTrue(result == (42, 42))
      } @@ ignore,
      test("retry with exponential backoff and cap") {

        /** EXERCISE 4
          *
          * Implement a function that takes an effect, and returns an effect that will retry the original effect with
          * exponential backoff, up to a maximum of 10 times, and will cap the backoff at 1 second.
          *
          * If you know about zio.Schedule, you may NOT cheat and use it!
          */
        def autoretry[R, E, A](effect: ZIO[R, E, A], maxRetries: Int = 10): ZIO[R, E, A] =          
          effect.exit.flatMap { exit =>
            if exit.isFailure then 
              val retried = 
                ZIO.iterate[R, Nothing, (Int, Exit[E, A])]((1, exit))(t => t._1 < maxRetries && t._2.isFailure):
                  case (iterations, exit) => 
                    ZIO.sleep(Math.pow(2, iterations).toInt.millis) *> effect.exit.map(exit => (iterations + 1, exit))

              retried.map(_._2).flatten
            else exit            
          }

        for result <- autoretry(ZIO.fail(new Error("Uh oh!"))).flip
        yield assertTrue(result.isInstanceOf[Error])
      } @@ ignore @@ TestAspect.withLiveClock,
      test("effect, interrupted") {

        /** EXERCISE 5
          *
          * The effect returned by this function is being interrupted, which is causing the ref to not be updated. Fix
          * the issue so that the ref is updated even if the effect is interrupted.
          */
        def makeEffect(promise: Promise[Nothing, Unit], ref: Ref[Int]) =
          promise.succeed(()).delay(10.millis).ensuring(ref.update(_ + 1))

        for
          promise <- Promise.make[Nothing, Unit]
          ref     <- Ref.make(0)
          fiber   <- makeEffect(promise, ref).fork
          _       <- promise.await
          _       <- fiber.interrupt
          result  <- ref.get
        yield assertTrue(result == 1)
      } @@ ignore @@ TestAspect.withLiveClock,
      test("flawless handoff") {
        def doWork(i: Int): ZIO[Any, Exception, Int] = ZIO.succeed(i)

        /** EXERCISE 6
          *
          * Implement the `startWorker` function so that it allows interruption while waiting for more work, but not
          * otherwise; and that if the `doWork` function fails to handle an item, then the item is returned to the
          * queue, so that no work is dropped.
          *
          * Note that the test does NOT check for correct implementation.
          */
        def startWorker(queue: Queue[Int]): ZIO[Any, Nothing, Fiber[Nothing, Nothing]] =
          ZIO.uninterruptibleMask { restore =>
            restore(queue.take).flatMap(item => doWork(item).orElse(queue.offer(item)))
          }.forever.fork

        for
          queue  <- Queue.bounded[Int](100)
          _      <- Random.nextInt.flatMap(queue.offer(_)).repeatN(99)
          fibers <- ZIO.foreach(1 to 10)(_ => startWorker(queue))
          fiber  <- Fiber.collectAll(fibers).interrupt
        yield assertCompletes
      } @@ ignore,
      test("flawless test of flawless handoff") {
        def doWork(i: Int, processed: Ref[Set[Int]]): ZIO[Any, Exception, Unit] = 
          zio.Random.nextDouble.flatMap: 
            case x if x < 0.9 => processed.update(_ + i)
            case _ => ZIO.fail(new Exception(s"Failed to process $i"))

        def startWorker(queue: Queue[Int], processed: Ref[Set[Int]]): ZIO[Any, Nothing, Fiber[Nothing, Nothing]] =
          ZIO.uninterruptibleMask { restore =>
            restore(queue.take).flatMap(item => doWork(item, processed).orElse(queue.offer(item)))
          }.forever.fork

        def trialRun(queue: Queue[Int], processed: Ref[Set[Int]]) = 
          for 
            fibers <- ZIO.foreach(0 to 10)(_ => startWorker(queue, processed))
            fiber  <- ZIO.foreach(fibers)(_.interrupt)
            size   <- processed.get.map(_.size)
          yield size


        /** EXERCISE 7
          *
          * Recreate the test above, but this time, develop a test that will succeed only if the implementation is
          * correct.
          *
          * You will have to make some changes to `doWork` and `startWorker`.
          */
        for
          ref    <- Ref.make(Set.empty[Int])
          queue  <- Queue.bounded[Int](100)
          _      <- ZIO.foreach(0 to 1000)(queue.offer(_)).fork
          _      <- trialRun(queue, ref).repeatUntil(_ >= 1000)
        yield assertCompletes
      } @@ TestAspect.withLiveRandom
    )
