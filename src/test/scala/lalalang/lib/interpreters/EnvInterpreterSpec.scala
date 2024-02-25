package lalalang.lib.interpreters

import cats.effect.IO
import cats.syntax.all.*
import lalalang.examples.functions.{diverging, fact, fibDirect}
import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.EnvInterpreter.{Err, Value}
import munit.FunSuite
import lalalang.lib.util.IOSuite

class EnvInterpreterSpec extends FunSuite with IOSuite:
  val interpreter =
    EnvInterpreter[IO](debug = false)

  def eval = interpreter.initEval(Map.empty)

  testIO("factorial") {
    val testCases = List(
      -1 -> 1,
      0  -> 1,
      1  -> 1,
      2  -> 2,
      3  -> 6,
      10 -> 3628800
    )

    testCases
      .traverse { (in, out) =>
        eval(fact(in))
          .map(assertEquals(Value.Number(out), _))
      }
  }

  testIO("fibonacci") {
    val testCases = List(1 -> 1, 2 -> 2, 10 -> 89)

    testCases
      .traverse { (in, out) =>
        eval(fibDirect(in))
          .map(assertEquals(Value.Number(out), _))
      }
  }

  testIO("blackhole") {
    eval(diverging).attempt
      .map {
        case Left(e)  => assertEquals(e, Err.Unevaluated("x"))
        case Right(_) => fail("should have raised")
      }
  }
