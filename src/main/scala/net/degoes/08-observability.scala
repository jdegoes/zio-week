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
import zio.internal.FiberRuntime

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

        def traces(cause: Cause[String]): List[StackTrace] = 
          cause.traces

        Live.live(for {
          cause <- async.sandbox.flip
          ts     = traces(cause)
          _     <- ZIO.debug(cause.prettyPrint)
        } yield assertTrue(ts(0).stackTrace.length > 0))
      }
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
          _          <- Fiber.dumpAllWith(dump => ZIO.debug(dump))
          supervisor <- Supervisor.track(false)
          _          <- example.supervised(supervisor)
          children   <- supervisor.value
          _          <- ZIO.foreach(children)(child => child.dump.debug)
        } yield assertTrue(children.length == 2)
      },
    )


object LoggingSpec extends ZIOSpecDefault:
  def spec = suite("LoggingSpec")(
    test("basic") {

      /** EXERCISE
        *
        * Use `ZIO.logInfo` to log a message at the `Info` level.
        */
      lazy val logged: ZIO[Any, Nothing, Unit] =
        ZIO.log("Hello World!")

      for _ <- ZIO.logLevel(LogLevel.Info)(logged)
      yield assertCompletes
    } @@ ignore,
    test("log error cause") {
      val cause = Cause.fail("Uh oh!")

      /** EXERCISE
        *
        * Use `ZIO.logErrorCause` to log an cause error message at the `Error` level.
        */
      lazy val logged: ZIO[Any, Nothing, Unit] =
        ZIO.logErrorCause("The connection was refused", Cause.die(new Error("Connection refused")))

      for 
        _      <- logged
        output <- ZTestLogger.logOutput
        _      <- ZIO.debug(output.mkString("LOG LINE", "\n", ""))
      yield assertCompletes
    },
    test("aspect logging") {
      val effect = 
        ZIO.logAnnotate("CorrelationId", "DLKJSDF") {
          for 
            _ <- Console.printLine("Hello World")
            _ <- ZIO.logInfo("logging at info")
          yield ()
        }

      ZIO.logSpan("database processing") {
        for 
          _ <- Console.printLine("Hello World")
          _ <- ZIO.logInfo("logging at info")
        yield ()
      }
      /** EXERCISE
        *
        * Use `ZIOAspect.logged` to create an aspect that can be used for logging effects.
        */
      val logged = ZIOAspect.logged("Aspect Loggign")

      val all = logged @@ ZIOAspect.debug 

      for _ <- ZIO.succeed(42) @@ all
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
      val counter: Metric.Counter[Long] = 
        Metric.counter("failed-requests").tagged(Set(MetricLabel("region", "east")))

      val failedRequests = counter.trackErrorWith(_ => 1L)

      val duration = Metric.histogram("request-duration", ???)

      val requestDuration = duration.trackDurationWith(duration => duration.toMillis().toDouble)

      val bothMetrics = failedRequests @@ requestDuration

      val makeRequest = ZIO.unit 

      for 
        _ <- makeRequest @@ bothMetrics
        _ <- counter.update(1L)
        value <- counter.value.map(_.count)
      yield assertCompletes
    } @@ ignore,
    test("gauge") {

      /** EXERCISE
        *
        * Create a gauge using `Metric.gauge`.
        */
      lazy val gauge: Metric.Gauge[Double] = Metric.gauge("memory-available")

      for 
        _ <- gauge.update(42)
        value <- gauge.value.map(_.value)
      yield assertCompletes
    } @@ ignore,
  )
