package lalalang.lib.interpreters.bytecode

import lalalang.lib.expr.ArithmeticFn.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.ComparisonFn.*
import lalalang.lib.expr.*

import scala.collection.mutable

type Bytecode = List[Instr]

object Bytecode:
  def generate(expr: Expr): Bytecode =
    val bb = BytecodeBuilder()
    bb.generate(expr)
    bb.build

  def eval(expr: Expr): VM.Value =
    val bc = generate(expr)
    val vm = VM(bc)
    vm.execute
    vm.currentValue.get

enum IntBinOp:
  case Add, Sub, Mul, Div, Lt, Eq, Gt

enum Instr:
  case Halt
  case IntConst(v: Int)
  case IntBinOpInstr(op: IntBinOp)
  // case IntUnaryOpInstr(op: IntBinOp)

object IntBinOp:
  def fromArithmeticFn: ArithmeticFn => IntBinOp =
    case ArithmeticFn.Add => Add
    case ArithmeticFn.Sub => Sub
    case ArithmeticFn.Mul => Mul
    case ArithmeticFn.Div => Div

  def fromComparisonFn: ComparisonFn => IntBinOp =
    case ComparisonFn.Lt => Lt
    case ComparisonFn.Eq => Eq
    case ComparisonFn.Gt => Gt

class BytecodeBuilder():
  private val bytecodeBuffer = mutable.ArrayBuffer.empty[Instr]

  private def emitInstr(instr: Instr): Unit =
    bytecodeBuffer.addOne(instr)

  def generate(expr: Expr): Unit =
    expr match
      case Expr.Lit(x) =>
        emitInstr(Instr.IntConst(x))

      case Expr.Builtin(BuiltinFn.Arithmetic(fn, a, b)) =>
        generate(a)
        generate(b)
        val opType = IntBinOp.fromArithmeticFn(fn)
        emitInstr(Instr.IntBinOpInstr(opType))

      case Expr.Builtin(BuiltinFn.Comparison(fn, a, b)) =>
        generate(a)
        generate(b)
        val opType = IntBinOp.fromComparisonFn(fn)
        emitInstr(Instr.IntBinOpInstr(opType))
      case _ => ???

  def build: List[Instr] =
    bytecodeBuffer.toList
end BytecodeBuilder
