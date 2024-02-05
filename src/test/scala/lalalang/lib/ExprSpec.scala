package lalalang
package lib

import lalalang.examples.functions.*
import lalalang.examples.functions.bool.*
import lalalang.lib.expr.{BuiltinFn, ComparisonFn, Expr}
import lalalang.lib.interpreters.TreeInterpreter
import lalalang.lib.interpreters.TreeInterpreter.Error
import munit.{FunSuite, ScalaCheckSuite}

class ExprSpec extends FunSuite with ScalaCheckSuite:

  def eval(e: Expr): Expr =
    TreeInterpreter.eval[Either[Error, *]](e).toOption.get

  test("T && F should be equal to TFT") {
    assert(eval(tft) == eval(andtf))
  }

  test("Cond should return reduced true branch for true predicate") {
    val expr = Expr.Cond(
      Expr.Builtin(
        BuiltinFn.Comparison(
          ComparisonFn.Eq,
          Expr.Lit(1),
          Expr.Lit(1)
        )
      ),
      incApply(42),
      Expr.Lit(0)
    )

    assert(eval(expr) == Expr.Lit(43))
  }

  test("Cond should return reduced false branch for false predicate") {
    val expr = Expr.Cond(
      Expr.Builtin(
        BuiltinFn.Comparison(
          ComparisonFn.Eq,
          Expr.Lit(1),
          Expr.Lit(0)
        )
      ),
      incApply(42),
      Expr.Lit(0)
    )

    assert(eval(expr) == Expr.Lit(0))
  }

  test("Should calculate fibonacci") {
    val testCases = Map(1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 5, 5 -> 8, 10 -> 89)
    testCases
      .foreach { (in, expected) =>
        assert(eval(fib(in, _lazy = true)) == Expr.Lit(expected))
      }
  }
