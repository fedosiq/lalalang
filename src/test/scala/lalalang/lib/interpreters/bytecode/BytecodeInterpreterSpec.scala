package lalalang.lib.interpreters.bytecode

import munit.FunSuite
import lalalang.examples.functions.twoPlus3Times4
import lalalang.lib.expr.dsl.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.Var
import lalalang.lib.expr.dsl.Conversions.{given Conversion[Int, Expr.Lit]}

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
