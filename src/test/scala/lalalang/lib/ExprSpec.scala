package lalalang
package lib

import munit.{ScalaCheckSuite, FunSuite}
import lalalang.lib.interpreters.SubstituteTreeInterpreter
import lalalang.lib.expr.Expr
import lalalang.lib.expr.BuiltinFn
import lalalang.lib.expr.ComparisonFn

class ExprSpec extends FunSuite with ScalaCheckSuite:
  import lalalang.functions.*
  import lalalang.functions.booleans.*

  test("T && F should be equal to TFT") {
    assert(SubstituteTreeInterpreter.eval(tft) == SubstituteTreeInterpreter.eval(andtf))
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

    assert(SubstituteTreeInterpreter.eval(expr) == Expr.Lit(43))
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

    assert(SubstituteTreeInterpreter.eval(expr) == Expr.Lit(0))
  }

  test("Should calculate fibonacci") {
    val testCases = Map(1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 5, 5 -> 8, 10 -> 89)
    testCases
      .foreach { (in, expected) =>
        assert(SubstituteTreeInterpreter.eval(fib(in, _lazy = true)) == Expr.Lit(expected))
      }
  }
