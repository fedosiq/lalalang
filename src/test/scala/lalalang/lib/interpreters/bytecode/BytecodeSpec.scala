package lalalang.lib.interpreters.bytecode

import munit.FunSuite
import lalalang.lib.expr.dsl.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.Var
import lalalang.lib.expr.dsl.Conversions.{given Conversion[Int, Expr.Lit]}
import lalalang.lib.interpreters.bytecode.Instr.*

class BytecodeSpec extends FunSuite:
  test("Bytecode generation") {
    val expr = let("y" -> 11).in {
      let("x" -> add(3, 1))
        .in(mul(Var("x"), Var("y")))
    }

    val res = Bytecode.generate(expr)

    val expected = List(
      Instr.IntConst(11),
      EnvSave(0),
      IntConst(3),
      IntConst(1),
      IntBinOpInstr(IntBinOp.Add),
      EnvSave(1),
      EnvLoad(1),
      EnvLoad(0),
      IntBinOpInstr(IntBinOp.Mul),
      EnvRestore(1),
      EnvRestore(0)
    )

    assertEquals(res, expected)
  }
