package lalalang.lib.interpreters.bytecode

import lalalang.examples.functions.{lambda2, twoPlus3Times4}
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.{App, Var}
import lalalang.lib.expr.dsl.*
import lalalang.lib.expr.dsl.Conversions.given Conversion[Int, Expr.Lit]
import munit.FunSuite

class BytecodeInterpreterSpec extends FunSuite:
  test("Arithmetics") {
    assertEquals(Bytecode.eval(twoPlus3Times4), VM.Value.Integer(14))
  }

  test("Arithmetics with bindings") {
    val expr = let("y" -> 11).in {
      let("x" -> add(3, 1))
        .in(mul(Var("x"), Var("y")))
    }

    assertEquals(Bytecode.eval(expr), VM.Value.Integer(44))
  }

  test("Lambdas") {
    val expr =
      let("f" -> lambda2(("a", "b"), Var("a") * Var("b")))
        .in(App(App(Var("f"), 11), 3))

    assertEquals(Bytecode.eval(expr), VM.Value.Integer(33))
  }
