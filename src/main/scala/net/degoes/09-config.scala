package net.degoes.config

import zio._
import zio.test._
import zio.test.TestAspect._

object ConfigSpec extends ZIOSpecDefault:
  def spec = suite("ConfigSpec")()
