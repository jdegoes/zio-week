package net.degoes.logging

import zio._
import zio.test._
import zio.test.TestAspect._

object LoggingSpec extends ZIOSpecDefault:
  def spec = suite("LoggingSpec")()
