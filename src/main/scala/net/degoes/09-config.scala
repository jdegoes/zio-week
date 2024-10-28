/** CONFIGURATION
  *
  * Configuration is a common concern in software development. It is the set of parameters that define the behavior of a
  * system, and it can be used to customize the behavior of a system without changing its code.
  *
  * In this section, you'll explore how to use ZIO to manage configuration in a type-safe and composable way.
  */
package net.degoes.config

import zio._
import zio.test._
import zio.test.TestAspect._

object ConfigSpec extends ZIOSpecDefault:
  def spec = suite("ConfigSpec")(
    test("primitive") {

      /** EXERCISE 1
        *
        * Create a `Config[Int]` that reads an integer from the configuration using the key name "port".
        */
      lazy val config: Config[Int] = ???

      val provider = ConfigProvider.fromMap(Map("port" -> "8080"))

      for value <- provider.load(config)
      yield assertTrue(value == 8080)
    } @@ ignore,
    test("composite") {
      case class ServerConfig(port: Int, host: String)

      /** EXERCISE 2
        *
        * Create a `Config[ServerConfig]` that reads a `ServerConfig` from the configuration.
        */
      lazy val port: Config[Int]    = ???
      lazy val host: Config[String] = ???

      lazy val config: Config[ServerConfig] =
        ???

      val provider = ConfigProvider.fromMap(Map("port" -> "8080", "host" -> "localhost"))

      for value <- provider.load(config)
      yield assertTrue(value == ServerConfig(8080, "localhost"))
    } @@ ignore,
    test("from layer") {
      case class ServerConfig(port: Int, host: String)

      given Config[ServerConfig] =
        (Config.int("port") ++ Config.string("host")).map:
          case (l, r) => ServerConfig(l, r)

      val provider = ConfigProvider.fromMap(Map("port" -> "8080", "host" -> "localhost"))

      /** EXERCISE 3
        *
        * Using `Runtime.setConfigProvider`, construct a layer that will set the default config provider to the above
        * provider.
        *
        * Then be sure to apply this layer to the test effect.
        */
      lazy val layer = Runtime.setConfigProvider(provider)

      for value <- ZIO.config[ServerConfig]
      yield assertTrue(value == ServerConfig(8080, "localhost"))
    } @@ ignore,
  )
