package lalalang
package lib

import lalalang.examples.functions.*
import lalalang.examples.functions.bool.*
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.add
import lalalang.lib.expr.{BuiltinFn, ComparisonFn, Expr}
import lalalang.lib.interpreters.TreeInterpreter
import lalalang.lib.interpreters.TreeInterpreter.Error
import munit.FunSuite

class TreeInterpreterSpec extends FunSuite:
  def evalEither(e: Expr) =
    TreeInterpreter.eval[Either[Error, *]](e)

  def eval(e: Expr): Expr =
    evalEither(e).toOption.get

  test("T && F should be equal to TFT") {
    assert(eval(tft) == eval(andtf))
  }

  test("Cond should return reduced true branch for true predicate") {
    val expr = Cond(
      Builtin(
        BuiltinFn.Comparison(
          ComparisonFn.Eq,
          Lit(1),
          Lit(1)
        )
      ),
      incApply(42),
      Lit(0)
    )

    assert(eval(expr) == Lit(43))
  }

  test("Cond should return reduced false branch for false predicate") {
    val expr = Cond(
      Builtin(
        BuiltinFn.Comparison(
          ComparisonFn.Eq,
          Lit(1),
          Lit(0)
        )
      ),
      incApply(42),
      Lit(0)
    )

    assert(eval(expr) == Lit(0))
  }

  test("Should calculate fibonacci") {
    val testCases = List(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 5, 5 -> 8, 10 -> 89)
    testCases
      .foreach { (in, expected) =>
        assert(eval(fib(in, _lazy = true)) == Lit(expected))
      }
  }

  test("Should raise UnsupportedOp error for binding") {
    val expr = Bind(Binding(true, "x", add(Var("x"), lit(1))), Var("f"))
    assertEquals(evalEither(expr), Left(TreeInterpreter.Error.UnsupportedOp("binding")))
  }
