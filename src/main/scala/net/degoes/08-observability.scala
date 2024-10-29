/** OBSERVABILITY
  *
  * Observability is the ability to understand the internal state of a system based on its external outputs. It is a
  * critical aspect of building and maintaining complex systems, and is essential for debugging, monitoring, and
  * optimizing applications.
  *
  * In this section, you'll explore the basics of observability with ZIO, focusing on logging and async stack traces.
  */
package net.degoes.observability

import zio._
import zio.test._
import zio.test.TestAspect._

object AsyncTraces extends ZIOSpecDefault {
  def spec =
    suite("AsyncTraces") {

      /** EXERCISE 1
        *
        * Pull out the `traces` associated with the following sandboxed failure, and verify there is at least one trace
        * element.
        */
      test("traces") {
        def async =
          for {
            _ <- ZIO.sleep(1.millis)
            _ <- ZIO.fail("Uh oh!")
          } yield ()

        def traces(cause: Cause[String]): List[StackTrace] = ???

        Live.live(for {
          cause <- async.sandbox.flip
          ts     = traces(cause)
        } yield assertTrue(ts(0).stackTrace.length > 0))
      } @@ ignore
    }
}

object TracesSpec extends ZIOSpecDefault:
  def spec =
    suite("TracesSpec")(
      test("traces") {
        def async =
          for {
            _ <- ZIO.sleep(1.millis)
            _ <- ZIO.fail("Uh oh!")
          } yield ()

        /** EXERCISE 2
          *
          * Pull out the `traces` associated with the following sandboxed failure, and verify there is at least one
          * trace element.
          */
        def traces(cause: Cause[String]): List[StackTrace] = ???

        Live.live(for {
          cause <- async.sandbox.flip
          ts     = traces(cause)
        } yield assertTrue(ts(0).stackTrace.length > 0))
      } @@ ignore,
      test("dump") {
        val example =
          for {
            promise <- Promise.make[Nothing, Unit]
            blocked <- promise.await.forkDaemon
            child1  <- ZIO.foreach(1 to 100000)(_ => ZIO.unit).forkDaemon
          } yield ()

        /** EXERCISE 3
          *
          * Compute and print out all fiber dumps of the fibers running in this test. Note that you should also explore
          * `Fiber.dumpAll` and `Fiber.dumpAllWith`.
          */
        for {
          supervisor <- Supervisor.track(false)
          _          <- example.supervised(supervisor)
          children   <- supervisor.value
          _          <- ZIO.foreach(children)(child => ZIO.unit)
        } yield assertTrue(children.length == 2)
      } @@ ignore,
    )

object LoggingSpec extends ZIOSpecDefault:
  def spec = suite("LoggingSpec")(
    test("basic") {

      /** EXERCISE
        *
        * Use `ZIO.logInfo` to log a message at the `Info` level.
        */
      lazy val logged: ZIO[Any, Nothing, Unit] =
        ???

      for _ <- logged
      yield assertCompletes
    } @@ ignore,
    test("log error cause") {
      val cause = Cause.fail("Uh oh!")

      /** EXERCISE
        *
        * Use `ZIO.logErrorCause` to log an cause error message at the `Error` level.
        */
      lazy val logged: ZIO[Any, Nothing, Unit] =
        ???

      for _ <- logged
      yield assertCompletes
    } @@ ignore,
    test("aspect logging") {

      /** EXERCISE
        *
        * Use `ZIOAspect.logged` to create an aspect that can be used for logging effects.
        */
      lazy val logged: ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
        ???

      for _ <- ZIO.succeed(42) @@ logged
      yield assertCompletes
    } @@ ignore,
  )

object MetricsSpec extends ZIOSpecDefault:
  import zio.metrics._

  def spec = suite("MetricsSpec")(
    test("counter") {

      /** EXERCISE
        *
        * Create a counter using `Metric.counter`.
        */
      lazy val counter: Metric.Counter[Long] = ???

      for _ <- counter.update(1)
      yield assertCompletes
    } @@ ignore,
    test("gauge") {

      /** EXERCISE
        *
        * Create a gauge using `Metric.gauge`.
        */
      lazy val gauge: Metric.Gauge[Double] = ???

      for _ <- gauge.update(42)
      yield assertCompletes
    } @@ ignore,
  )
