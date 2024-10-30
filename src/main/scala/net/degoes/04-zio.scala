/** ZIO
  *
  * ZIO is a mature, production-grade effect system in Scala, whose innovations inspried the Effect TS ecosystem and
  * ports in many other languages.
  *
  * Unlike previous generation effect systems, which tended toward abstract category theory, ZIO significantly improves
  * developer experience without compromising on the power of functional effects.
  *
  * ZIO's batteries-included philosophy means more joy, less pain.
  *
  * In this section, you'll learn the basics of ZIO.
  */
package net.degoes.zio

import zio._
import zio.test._
import zio.test.TestAspect.ignore

object Types:
  type ??? = Nothing

  // ZIO has three type parameters:
  //
  // R: The environment type, which describes context the effect requires.
  // A: The success type, which describes the type of a successful execution.
  // E: The failure type, which describes the type of a failed execution.

  type Request
  type Response
  type StatusCode

  /** EXERCISE 1
    *
    * Make the type for an effect that requires a `Request`, fails with a `StatusCode`, and succeeds with a `Response`.
    */
  type RequestHandler = ZIO[Request, StatusCode, Response]

  trait SparkContext
  type Dataset[A]

  /** EXERCISE 2
    *
    * Make the type for an effect that requires a `SparkContext`, can fail with a `Throwable`, and succeeds with a
    * `Dataset`.
    */
  type Job = ???

object IntroSpec extends ZIOSpecDefault:
  def spec =
    suite("IntroSpec")(
      test("succeed") {

        /** EXERCISE 3
          *
          * Using `ZIO.succeed`, construct an effect that succeeds with the value 42.
          */
        lazy val effect: ZIO[Any, Nothing, Int] = ZIO.succeed(42)

        for success <- effect
        yield assertTrue(success == 42)
      } @@ ignore,
      test("fail") {

        /** EXERCISE 4
          *
          * Using `ZIO.fail`, construct an effect that fails with the value "Uh oh".
          */
        lazy val effect: ZIO[Any, String, Nothing] = ZIO.fail("Uh oh")

        for error <- effect.flip
        yield assertTrue(error == "Uh oh")
      } @@ ignore,
      test("ZIO.attempt - successful") {

        /** EXERCISE 5
          *
          * Using `ZIO.attempt`, construct an effect that succeeds with the value 42.
          */
        lazy val effect: ZIO[Any, Throwable, Int] = ZIO.attempt(42)

        for success <- effect
        yield assertTrue(success == 42)
      } @@ ignore,
      test("ZIO.attempt - failure") {

        /** EXERCISE 6
          *
          * Using `ZIO.attempt`, construct an effect from throwing an exception with the message "Uh oh".
          */
        lazy val effect: ZIO[Any, Throwable, Nothing] = ZIO.attempt(throw new Exception("Uh oh"))

        for error <- effect.flip
        yield assertTrue(error.getMessage == "Uh oh")
      } @@ ignore,
      test("ZIO#map") {
        val effect = ZIO.succeed(42)

        /** EXERCISE 7
          *
          * Using `ZIO#map`, transform the effect to add 1 to the value.
          */
        lazy val mapped: ZIO[Any, Nothing, Int] = effect.map(_ + 1)

        for result <- mapped
        yield assertTrue(result == 43)
      } @@ ignore,
      test("ZIO#mapError") {
        val effect = ZIO.fail("Uh oh")

        /** EXERCISE 8
          *
          * Using `ZIO#mapError`, transform the effect to get the length of the error message.
          */
        lazy val mapped: ZIO[Any, Int, Nothing] = effect.mapError(_.length)

        for error <- mapped.flip
        yield assertTrue(error == 5)
      } @@ ignore,
      test("ZIO#flatMap") {
        val effect = ZIO.succeed(42)

        /** EXERCISE 9
          *
          * Using `ZIO#flatMap`, transform the effect to add 1 to the value.
          */
        lazy val flatMapped: ZIO[Any, Nothing, Int] = 
          for 
            value <- effect 
            result <- ZIO.succeed(value + 1)
          yield result

        for result <- flatMapped
        yield assertTrue(result == 43)
      } @@ ignore,
      test("ZIO#catchAll") {
        val effect = ZIO.fail("Uh oh")

        /** EXERCISE 10
          *
          * Using `ZIO#catchAll`, transform the effect to get the length of the error message.
          */
        lazy val caught: ZIO[Any, Nothing, Int] = 
          effect.catchAll(error => ZIO.succeed(error.length))

        for len <- caught
        yield assertTrue(len == 5)
      } @@ ignore,
      test("ZIO.(*>)") {
        var i = 0

        val increment = ZIO.succeed { i += 1; i }
        val decrement = ZIO.succeed { i -= 1; i }

        /** EXERCISE 11
          *
          * Using `ZIO.(*>)`, combine the `increment` and `decrement` effects to increment twice and decrement once.
          */
        lazy val incIncDec: ZIO[Any, Nothing, Int] = 
          increment <* ZIO.logInfo("Incremented") *> increment *> decrement

        for result <- incIncDec
        yield assertTrue(result == 1)
      } @@ ignore,
      test("ZIO#zip") {
        val first  = ZIO.succeed(42)
        val second = ZIO.succeed("foo")

        /** EXERCISE 12
          *
          * Using `ZIO#zip`, combine the `first` and `second` effects to produce a tuple of their results.
          */
        lazy val zipped: ZIO[Any, Nothing, (Int, String)] = first.zipPar(second)

        for result <- zipped
        yield assertTrue(result == (42, "foo"))
      } @@ ignore,
      test("ZIO.foreach") {
        val urls = List("http://url1.com", "http://url2.com", "http://url3.com")

        final case class Response(body: String)

        def doRequest(url: String): ZIO[Any, Nothing, Response] =
          ZIO.succeed(Response(s"Response from $url"))

        /** EXERCISE 13
          *
          * Using `ZIO.foreach`, transform the list of URLs into a list of responses.
          */
        lazy val responses: ZIO[Any, Nothing, List[Response]] = 
          ZIO.foreachPar(urls)(doRequest)

        for result <- responses
        yield assertTrue(
          result == List(
            Response("Response from http://url1.com"),
            Response("Response from http://url2.com"),
            Response("Response from http://url3.com"),
          )
        )
      } @@ ignore,
      test("ZIO.service") {
        trait Logger {
          def log(line: String): ZIO[Any, Nothing, Unit] = ZIO.succeed(println(line))
        }
        object Logger extends Logger

        /** EXERCISE 14
          *
          * Using `ZIO.service`, create an effect that requires a value of type `Logger` from the environment. Do not
          * worry about the specifics of providing the effect its required context, as that is done for you, and will be
          * explored in more detail later.
          */
        val effect = 
          for
            logger <- ZIO.service[Logger]
            _      <- logger.log("Hello, world!")
          yield assertTrue(true)
          
        effect.provide(ZLayer.succeed(Logger))
      },
    )

object ZIOConcurrencySpec extends ZIOSpecDefault:
  def spec =
    suite("ZIOConcurrencySpec")(
      suite("Core")(
        test("ZIO#race") {

          /** EXERCISE 15
            *
            * Using `ZIO#race`, race two effects, such that the winner ends up succeeding with a value of 42.
            */
          lazy val effect: ZIO[Any, Any, Int] = 
            ZIO.fail("24").race(ZIO.succeed(42))

          for result <- effect
          yield assertTrue(result == 42)

        } @@ ignore,
        test("ZIO#timeout") {

          /** EXERCISE 16
            *
            * Using `ZIO#timeout`, timeout an effect that takes 1 second to complete after 500 milliseconds.
            *
            * You can use `ZIO.sleep` to sleep for a specified duration.
            */
          lazy val effect: ZIO[Any, Any, Option[Int]] = 
            ZIO.sleep(1.second).as(42).timeout(500.millis)

          for result <- effect
          yield assertTrue(result == None)
        } @@ ignore,
        test("ZIO#zipPar") {
          val first  = ZIO.succeed(42)
          val second = ZIO.succeed("foo")

          /** EXERCISE 17
            *
            * Using `ZIO#zipPar`, combine the `first` and `second` effects to produce a tuple of their results, where
            * execution of the individual effects occurs concurrently (in "PARallel").
            */
          lazy val zipped: ZIO[Any, Nothing, (Int, String)] = 
            first.zipPar(second)

          for result <- zipped
          yield assertTrue(result == (42, "foo"))
        } @@ ignore,
        test("ZIO.foreachPar") {
          val urls = List("http://url1.com", "http://url2.com", "http://url3.com")

          final case class Response(body: String)

          def doRequest(url: String): ZIO[Any, Nothing, Response] =
            ZIO.succeed(Response(s"Response from $url"))

          /** EXERCISE 18
            *
            * Using `ZIO.foreachPar`, transform the list of URLs into a list of responses, where the requests are made
            * concurrently.
            */
          lazy val responses: ZIO[Any, Nothing, List[Response]] = 
            ZIO.foreachPar(urls)(doRequest)

          for result <- responses
          yield assertTrue(
            result == List(
              Response("Response from http://url1.com"),
              Response("Response from http://url2.com"),
              Response("Response from http://url3.com"),
            )
          )
        } @@ ignore,
      ),
      suite("Ref")(
        test("Ref.make") {

          /** EXERCISE 19
            *
            * Using `Ref.make`, create a new `Ref` that is initialized to 0.
            */
          for ref <- Ref.make(0)
          yield assertTrue(ref != null)
        } @@ ignore,
        test("Ref#get") {

          /** EXERCISE 20
            *
            * Using `Ref#get`, get the value of the `Ref`.
            */
          for
            ref   <- Ref.make(42)
            value <- ref.get
          yield assertTrue(value == 42)
        } @@ ignore,
        test("Ref#set") {

          /** EXERCISE 21
            *
            * Using `Ref#set`, set the value of the `Ref` to 42.
            */
          for
            ref   <- Ref.make(0)
            _     <- ref.set(42)
            value <- ref.get
          yield assertTrue(value == 42)
        } @@ ignore,
        test("Ref#update") {

          /** EXERCISE 22
            *
            * Using `Ref#update`, increment the value of the `Ref` by 1.
            */
          for
            ref   <- Ref.make(0)
            _     <- ref.update(_ + 1)
            value <- ref.get
          yield assertTrue(value == 1)
        } @@ ignore,
        test("Ref#modify") {

          /** EXERCISE 23
            *
            * Using `Ref#modify`, increment the value of the `Ref` by 1 and return the old value.
            */
          for
            ref   <- Ref.make(0)
            old   <- ref.getAndUpdate(_ + 1)//modify(old => (old, old + 1))
            value <- ref.get
          yield assertTrue(old == 0) && assertTrue(value == 1)
        } @@ ignore,
      ),
      suite("Promise")(
        test("Promise.make") {

          /** EXERCISE 24
            *
            * Using `Promise.make`, create a new promise that can be used to produce an integer value.
            */
          for promise <- Promise.make[Nothing, Int]
          yield assertTrue(promise != null)
        } @@ ignore,
        test("Promise#succeed") {

          /** EXERCISE 25
            *
            * Using `Promise#succeed`, complete the specified promise with the value 42.
            */
          for
            promise <- Promise.make[Nothing, Int]
            _       <- promise.succeed(42)
            value   <- promise.await
          yield assertTrue(value == 42)
        } @@ ignore,
        test("Promise#fail") {

          /** EXERCISE 26
            *
            * Using `Promise#fail`, complete the specified promise with the error "Uh oh".
            */
          for
            promise <- Promise.make[String, Int]
            _       <- promise.fail("Uh oh")
            error   <- promise.await.flip
          yield assertTrue(error == "Uh oh")
        } @@ ignore,
        test("Promise#await") {

          /** EXERCISE 27
            *
            * Using `Promise#await`, await the result of the promise.
            */
          for
            promise <- Promise.make[String, Int]
            fiber   <- (ZIO.sleep(10.millis) *> promise.succeed(42)).fork
            value   <- promise.await
          yield assertTrue(value == 42)
        } @@ ignore,
      ),
      suite("Queue")(
        test("Queue.bounded") {

          /** EXERCISE 28
            *
            * Using `Queue.bounded`, create a new bounded queue that can hold up to 100 integers.
            */
          for queue <- Queue.bounded(100)
          yield assertTrue(queue != null)
        } @@ ignore,
        test("Queue#offer") {

          /** EXERCISE 29
            *
            * Using `Queue#offer`, offer the value 42 to the specified queue.
            */
          for
            queue <- Queue.bounded[Int](100)
            _     <- queue.offer(42)
          yield assertTrue(true)
        } @@ ignore,
        test("Queue#take") {

          /** EXERCISE 30
            *
            * Using `Queue#take`, take a value from the specified queue.
            */
          for
            queue <- Queue.bounded[Int](100)
            _     <- queue.offer(42)
            value <- queue.take
          yield assertTrue(value == 42)
        } @@ ignore,
        test("Queue#shutdown") {

          /** EXERCISE 31
            *
            * Using `Queue#shutdown`, shutdown the specified queue.
            */
          for
            queue    <- Queue.bounded[Int](100)
            _        <- queue.shutdown
            shutdown <- queue.isShutdown
          yield assertTrue(shutdown)
        } @@ ignore,
      ),
    ) @@ TestAspect.withLiveClock

object ZIOResourceSpec extends ZIOSpecDefault:
  def spec =
    suite("ZIOResourceSpec")(
      test("ZIO.acquireReleaseWith") {

        final case class File(path: String)

        @volatile var count = 0

        val acquire = ZIO.succeed { count += 1; File("foo.txt") }
        val release = (resource: File) => ZIO.succeed(count -= 1)

        /** EXERCISE 32
          *
          * Using `ZIO.acquireReleaseWith`, acquire a resource, use it, and then release it.
          */
        lazy val bracketed: ZIO[Any, Any, Unit] =
          ZIO.acquireReleaseWith(acquire)(release) { file =>        
            Console.printLine(s"Using $file")
          }

        for result <- bracketed
        yield assertTrue(count == 0)
      } @@ ignore,
      test("ZIO#ensuring") {
        @volatile var count = 0

        val incrementCounter = ZIO.succeed(count += 1)         

        import Console.printLine

        val left = 
          ZIO.succeed(42).ensuring(printLine("Finalizing 1").orDie)

        val right = 
          ZIO.succeed(24).ensuring(printLine("Finalizing 2").orDie)

        val finalizer = left.race(right).ensuring(Console.printLine("Race done").orDie)

        val child = 
          printLine("Hi from thread").ensuring(finalizer)

        for 
          fiber <- child.fork
        yield () 
        

        /** EXERCISE 33
          *
          * Using `ZIO#ensuring`, modify the following effect so that the effect `incrementCounter` is executed no
          * matter what.
          */
        lazy val ensuring: ZIO[Any, String, Int] =
          ZIO.fail("Uh oh!").ensuring(incrementCounter)

        for error <- ensuring.flip
        yield assertTrue(count == 1 && error == "Uh oh!")
      } @@ ignore,
      test("ZIO#onExit") {
        @volatile var count = 0

        val incrementCounter = ZIO.succeed(count += 1)

        /** EXERCISE 34
          *
          * Using `ZIO#onExit`, modify the following effect so that the effect `incrementCounter` is executed no matter
          * what.
          */
        lazy val onExit: ZIO[Any, String, Int] =
          ZIO.fail("Uh oh!")

        for error <- onExit.flip
        yield assertTrue(count == 1 && error == "Uh oh!")
      } @@ ignore,
      test("ZIO.addFinalizer + ZIO.scoped") {
        @volatile var count = 0

        val incrementCounter = ZIO.succeed(count += 1)

        /** EXERCISE 35
          *
          * Using `ZIO.addFinalizer`, add a finalizer which just increments the counter. Add this finalizer in the first
          * line of the for comprehension.
          *
          * This will introduce a `Scope` dependency in your effect, which you should eliminate using `ZIO.scoped`.
          */

        (for
          _     <- ZIO.addFinalizer(incrementCounter)
          error <- ZIO.fail("Uh oh!").flip
        yield error).flatMap(error => assertTrue(count == 1 && error == "Uh oh!"))
      } @@ ignore,
    )
