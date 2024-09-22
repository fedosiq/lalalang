package lalalang.lib.interpreters.bytecode

import lalalang.examples.functions.lambda2
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.Var
import lalalang.lib.expr.dsl.*
import lalalang.lib.expr.dsl.Conversions.given Conversion[Int, Expr.Lit]
import lalalang.lib.interpreters.bytecode.Instr.*
import munit.FunSuite

class BytecodeSpec extends FunSuite:
  test("Bytecode generation 1") {
    val expr = let("y" -> 11).in {
      let("x" -> add(3, 1))
        .in(mul(Var("x"), Var("y")))
    }

    val res = Bytecode.generate(expr).instr

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
      EnvRestore(0),
      Halt
    )

    assertEquals(res, expected)
  }

  test("Bytecode generation 2".fail) {
    val expr = lambda2(("a", "b"), mul(Var("a"), Var("b")))

    val res = Bytecode.generate(expr).instr

    val expected = List(
      EnvLoad(0),
      EnvLoad(1),
      IntBinOpInstr(IntBinOp.Mul),
      Return,
      MakeClosure(1, 0),
      Return,
      MakeClosure(0, 0),
      Halt
    )

    assertEquals(res, expected)
  }
