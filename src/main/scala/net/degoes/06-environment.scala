/** ENVIRONMENT
  *
  * Second only to typed errors, ZIO's environment is one of its most distinguishing features. Providing a principled
  * means of ensuring resource safety, accomplishing compile-time safe dependency injection, and exposing local context
  * to functions that require it, the environment is a powerful tool in the ZIO toolkit.
  *
  * In this section, you will learn about ZEnvironment, basic environmental operations on effect, and dependency
  * injection with layers.
  */
package net.degoes.environment

import zio._
import zio.test._
import zio.test.TestAspect._

import java.io.IOException

object ZEnvSpec extends ZIOSpecDefault:
  trait Color:
    def red: Int
    def green: Int
    def blue: Int

  final case class Red() extends Color:
    val red   = 255
    val green = 0
    val blue  = 0

  final case class Green() extends Color:
    val red   = 0
    val green = 255
    val blue  = 0

  final case class Blue() extends Color:
    val red   = 0
    val green = 0
    val blue  = 255

  def spec =
    suite("ZEnvSpec")(
      test("get by type") {
        val all: ZEnvironment[Red & Green] =
          ZEnvironment(Red(), Green())

        /** EXERCISE 1
          *
          * Get the `Red` value from the `all` environment using the `ZEnvironment#get` method.
          *
          * Try to get `Blue`. What happens?
          */
        assertTrue(all.get[Red] == Red())
      } @@ ignore,
      test("add from empty") {
        val empty = ZEnvironment.empty

        /** EXERCISE 2
          *
          * Starting from empty, add `Red()` to the environment using the `ZEnvironment#add` method.
          */
        lazy val env: ZEnvironment[Red] = empty.add(Red())

        assertTrue(env.get[Red] == Red())
      } @@ ignore,
      test("union") {
        val redAndBlue = ZEnvironment.empty.add(Red()).add(Blue())
        val green      = ZEnvironment.empty.add(Green())

        /** EXERCISE 3
          *
          * Union the `redAndBlue` and `green` environments together to create an environment that contains all three
          * colors, using the `ZEnvironment#(++)` (union) method.
          */
        lazy val all: ZEnvironment[Red & Green & Blue] = redAndBlue ++ green

        assertTrue(all.get[Red] == Red() && all.get[Green] == Green() && all.get[Blue] == Blue())
      } @@ ignore,
      test("ZIO#provideEnvironment") {
        val zenv = ZEnvironment(Green(), Blue(), Red())

        val effect: ZIO[Green, IOException, Green] =
          ZIO.serviceWithZIO[Green](green => Console.printLine(green).as(green))

        /** EXERCISE 4
          *
          * Using `ZIO#provideEnvironment`, provide the `green` environment to this effect that requires a `Green`
          * value.
          */
        for green <- effect.provideEnvironment(zenv)
        yield assertTrue(green == Green())
      },
    )

/** Layers are recipes for building components of your application, given the appropriate input ingredients.
  *
  * Layers can be thought of as function that transform one `ZEnvironment` into another `ZEnvironment`:
  *
  * {{{
  * ZEnvironment[In] => Either[Err, ZEnvironment[Out]]
  * }}}
  *
  * The reality is a bit more nuanced, as layers may allocate resources which are safely released when their components
  * are no longer being used. In addition, the construction may happen asynchronously and concurrently.
  */
object ZLayerSpec extends ZIOSpecDefault:
  trait Printer:
    def print(str: String): ZIO[Any, Nothing, Unit]

  object Printer extends Printer:
    override def print(str: String): ZIO[Any, Nothing, Unit] = ZIO.debug(str)

  final case class Logger(printer: Printer):
    def log(line: String): ZIO[Any, Nothing, Unit] = printer.print(line)

  trait Config
  object Config extends Config

  trait Jdbc
  trait Metrics

  class UserRepo(jdbc: Jdbc, userConfig: Config, metrics: Metrics)
  object UserRepo:
    val layer = 
      ZLayer:
        for 
          jdbc    <- ZIO.service[Jdbc]
          cfg     <- ZIO.service[Config] 
          metrics <- ZIO.service[Metrics]
        yield UserRepo(jdbc, cfg, metrics)

  def spec =
    suite("ZLayerSpec")(
      test("succeed") {

        /** EXERCISE 5
          *
          * Using `ZLayer.succeed`, construct a layer that produces a ZEnvironment containing just a `Printer`.
          */
        lazy val layer: ZLayer[Any, Nothing, Printer] = 
          ZLayer.succeed(Printer)

        for env <- layer.build
        yield assertTrue(env.get[Printer] == Printer)
      } @@ ignore,
      test("from ZIO - no dependencies") {

        /** EXERCISE 6
          *
          * Using `ZLayer.apply`, the primary constructor for ZLayer, which expects a ZIO effect, construct a layer that
          * produces a ZEnvironment containing just a `Printer`.
          */
        val layer: ZLayer[Any, Nothing, Printer] =
          ZLayer {
            ZIO.succeed(Printer)
          }

        for env <- layer.build
        yield assertTrue(env.get[Printer] == Printer)
      } @@ ignore,
      test("from ZIO - with dependencies") {

        /** EXERCISE 7
          *
          * Using `ZLayer.apply`, the primary constructor for ZLayer, which expects a ZIO effect, construct a layer that
          * requires a `Printer` and uses that to produce a Logger.
          *
          * Note: You will have to use `ZIO.service[Printer]` to make an effect that requires a `Printer`, which can
          * then be used to construct the `Logger`.
          */
        val layer: ZLayer[Printer, Nothing, Logger] =
          ZLayer {
            for printer <- ZIO.service[Printer]
            yield Logger(printer)
          }

        (for env <- layer.build
        yield assertTrue(env.get[Logger] == Logger(Printer))).provideSome[Scope](ZLayer.succeed(Printer))
      } @@ ignore,
      test("ZIO.scoped") {

        /** EXERCISE 8
          *
          * Using `ZLayer.scoped`, create a layer that requires a `Printer` and uses that to produce a `Logger`. Using
          * `ZIO.addFinalizer`, add a finalizer that prints out "Logger being freed!". Note this finalizer will be
          * invoked when the logger is no longer used.
          */
        val layer: ZLayer[Printer, Nothing, Logger] =
          ZLayer.scoped {
            for 
              _       <- ZIO.addFinalizer(Console.printLine("Logger being freed!").ignore)
              printer <- ZIO.service[Printer]
            yield Logger(printer)
          }

        (for env <- layer.build
        yield assertTrue(env.get[Logger] == Logger(Printer))).provide(ZLayer.succeed(Printer) ++ Scope.default)
      } @@ ignore,
      test("ZIO#provide") {
        
        val configLayer: ZLayer[Any, Nothing, Config] = ZLayer.succeed(new Config{})

        val printerLayer: ZLayer[Config, Nothing, Printer] = 
          ZLayer {
            for 
              _ <- ZIO.service[Config]
            yield Printer
          }

        val loggerLayer: ZLayer[Printer, Nothing, Logger] =
          ZLayer.succeed(Logger(Printer))

        val effect: ZIO[Logger, IOException, Logger] =
          ZIO.serviceWithZIO[Logger](logger => Console.printLine(logger).as(logger))

        val baseLayer: ZLayer[Any, Nothing, Logger] = 
          ZLayer.make[Logger](loggerLayer, printerLayer, configLayer)

        /** EXERCISE 9
          *
          * Using `ZIO#provide`, provide a `Printer` to an effect that requires a `Logger`.
          */
        for logger <- effect.provide(baseLayer)
        yield assertTrue(logger == Logger(Printer))
      } @@ ignore,
      test("ZIO#provideSome") {
        val loggerLayer: ZLayer[Any, Nothing, Logger] =
          ZLayer.succeed(Logger(Printer))

        val configLayer: ZLayer[Any, Nothing, Config] =
          ZLayer.succeed(Config)

        val effect: ZIO[Config & Logger, IOException, Logger] =
          ZIO.serviceWithZIO[Logger](logger => Console.printLine(logger).as(logger))

        /** EXERCISE 9
          *
          * Using `ZIO#provideSome[Config]`, provide the logger to `effect`, leaving only `Config` as a requirement.
          */
        for logger <- effect.provideSome[Config](loggerLayer).provide(configLayer)
        yield assertTrue(logger == Logger(Printer))
      },
    )
