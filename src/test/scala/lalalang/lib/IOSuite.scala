package lalalang.lib

import cats.effect.IO
import munit.{FunSuite, Location}

trait IOSuite:
  self: FunSuite =>
  def testIO(name: String)(body: => IO[Any])(implicit loc: Location): Unit =
    test(name)(body.unsafeRunSync())
