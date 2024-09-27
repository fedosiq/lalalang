package lalalang.lib.interpreters.bytecode

import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.Var
import lalalang.lib.expr.dsl.*
import lalalang.lib.expr.dsl.Conversions.given Conversion[Int, Expr.Lit]
import lalalang.lib.interpreters.bytecode.Instr.*
import munit.FunSuite

class BytecodeGeneratorSpec extends FunSuite:
  test("Bindings") {
    val expr = let("y" -> 11).in {
      let("x" -> add(3, 1))
        .in(Var("x") * Var("y"))
    }

    val res = Bytecode.generate(expr).instr

    // property: same number of env saves and restores
    val expected = Vector(
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

  test("Lambdas") {
    val expr = Expr.App(Expr.App(lambda2(("a", "b"), Var("a") * Var("b")), lit(21)), lit(2))

    val res = Bytecode.generate(expr).instr

    val expected = Vector(
      IntConst(2),
      IntConst(21),
      MakeClosure(0, 1),
      Apply,
      Apply,
      Halt,
      MakeClosure(1, 2),
      Return,
      EnvLoad(0),
      EnvLoad(1),
      IntBinOpInstr(IntBinOp.Mul),
      Return
    )

    assertEquals(res, expected)
  }
