package net.degoes.http

import zio._
import zio.test._
import zio.test.TestAspect._

object HttpSpec extends ZIOSpecDefault:
  def spec = suite("HttpSpec")()
