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

object ZIOLimboSpec extends ZIOSpecDefault:
  def spec =
    suite("ZIOLimboSpec")(
      test("unrecoverable") {
        final case class Response(code: Int, body: String, headers: Map[String, String])

        final case class HttpException(code: Int, message: String) extends Exception(message)

        case class InsufficientFunds()

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
          for
            _ <- makeCharge
            _ <- recordInDatabase
          yield ()

        for result <- chargeUser.sandbox.flip
        yield assertTrue(result.defects.head.isInstanceOf[SQLException])
      } @@ ignore,
      test("looking for trouble") {
        final case class Response(code: Int, body: String, headers: Map[String, String])

        final case class HttpException(code: Int, message: String) extends Exception(message):
          def toResponse: Response = Response(code, message, Map("Content-Type" -> "text/plain"))

        /** EXERCISE 2
          *
          * Implement the `look` function so that it returns a new effect that that will succeed with a valid response
          * only if the original effect fails with an `HttpException` with a code of 404. Otherwise, the returned effect
          * should fail with `None`.
          */
        def look(effect: ZIO[Any, Throwable, Response]): ZIO[Any, Option[Nothing], Response] =
          ???

        for
          success <- look(ZIO.fail(HttpException(404, "Not Found")))
          failure <- look(ZIO.succeed(Response(200, "OK", Map.empty))).flip
        yield assertTrue(success.code == 404 && failure == None)
      } @@ ignore,
      test("poor man's zipPar") {

        /** EXERCISE 3
          *
          * Using `ZIO#fork` and `ZIO#join`, implement a function that takes two effects, and returns an effect that
          * concurrently executes both left and right effects, succeeding with a tuple of their results.
          */
        def zipPar[R, E, A](left: ZIO[R, E, A], right: ZIO[R, E, A]): ZIO[R, E, (A, A)] =
          ???

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
        def autoretry[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
          ???

        for result <- autoretry(ZIO.fail(new Error("Uh oh!"))).flip
        yield assertTrue(result.isInstanceOf[Error])
      } @@ ignore,
      test("effect, interrupted") {

        /** EXERCISE 5
          *
          * The effect returned by this function is being interrupted, which is causing the ref to not be updated. Fix
          * the issue so that the ref is updated even if the effect is interrupted.
          */
        def makeEffect(promise: Promise[Nothing, Unit], ref: Ref[Int]) =
          for
            _ <- promise.succeed(())
            _ <- ZIO.sleep(10.millis)
            _ <- ref.update(_ + 1)
          yield ()

        for
          promise <- Promise.make[Nothing, Unit]
          ref     <- Ref.make(0)
          fiber   <- makeEffect(promise, ref).fork
          _       <- promise.await
          _       <- fiber.interrupt
          result  <- ref.get
        yield assertTrue(result == 1)
      } @@ ignore,
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
        def startWorker(queue: Queue[Int]): ZIO[Any, Nothing, Fiber[Exception, Nothing]] =
          (for
            item <- queue.take
            _    <- doWork(item)
          yield ()).forever.fork

        for
          queue  <- Queue.bounded[Int](100)
          _      <- Random.nextInt.flatMap(queue.offer(_)).repeatN(100)
          fibers <- ZIO.foreach(1 to 10)(_ => startWorker(queue))
          fiber  <- Fiber.joinAll(fibers)
        yield assertCompletes
      } @@ ignore,
      test("flawless test of flawless handoff") {

        /** EXERCISE 7
          *
          * Recreate the test above, but this time, develop a test that will succeed only if the implementation is
          * correct.
          *
          * You will have to make some changes to `doWork` and `startWorker`.
          */
        assertTrue(false)
      } @@ ignore,
    )
