package lalalang.lib.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import munit.{FunSuite, Location}

trait IOSuite:
  self: FunSuite =>
  def testIO(name: String)(body: => IO[Any])(using Location): Unit =
    test(name)(body.unsafeRunSync())
