package lalalang
package lib

import munit.{ScalaCheckSuite, FunSuite}

class ExprSpec extends FunSuite with ScalaCheckSuite:
  import lalalang.functions.*
  import lalalang.functions.booleans.*

  test("T && F should be equal to TFT") {
    assert(Expr.eval(tft) == Expr.eval(andtf))
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

    assert(Expr.eval(expr) == Expr.Lit(43))
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

    assert(Expr.eval(expr) == Expr.Lit(0))
  }

  test("Should calculate fibonacci") {
    assert(Expr.eval(fib(1)) == Expr.Lit(1))
    assert(Expr.eval(fib(2)) == Expr.Lit(2))
    assert(Expr.eval(fib(3)) == Expr.Lit(3))
    assert(Expr.eval(fib(4)) == Expr.Lit(5))
    assert(Expr.eval(fib(5)) == Expr.Lit(8))
    assert(Expr.eval(fib(10)) == Expr.Lit(89))
  }
