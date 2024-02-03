package lalalang.lib.interpreters.bytecode

import lalalang.lib.expr.ArithmeticFn.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.ComparisonFn.*
import lalalang.lib.expr.*
import lalalang.lib.util.*

import scala.collection.mutable

enum IntBinOp:
  case Add, Sub, Mul, Div, Lt, Eq, Gt

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

enum Instr:
  case Halt
  case IntConst(v: Int)
  case IntBinOpInstr(op: IntBinOp)
  // case IntUnaryOpInstr(op: IntBinOp)

type Bytecode = List[Instr]

def generateBytecode(expr: Expr): Bytecode =
  val bb = BytecodeBuilder()
  bb.generate(expr)
  bb.build

def eval(expr: Expr): VM.Value =
  val bc = generateBytecode(expr)
  val vm = VM(bc)
  vm.execute
  vm.currentValue.get

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

class VM(bc: Bytecode):
  val stack: mutable.Stack[VM.Value] = mutable.Stack.empty

  var codePointer: Int = 0

  def execute: Unit =
    var shouldHalt = false

    while (codePointer < bc.length && !shouldHalt)
      val instr = bc(codePointer)
      codePointer += 1

      instr match
        case Instr.Halt =>
          shouldHalt = true
        case Instr.IntConst(v) =>
          stack.push(VM.Value.Integer(v))
        case Instr.IntBinOpInstr(op) =>
          val arg2 = stack.pop() |> VM.Value.asInt
          val arg1 = stack.pop() |> VM.Value.asInt

          val result = op match
            case IntBinOp.Add => arg1 + arg2
            case IntBinOp.Sub => arg1 - arg2
            case IntBinOp.Mul => arg1 * arg2
            case IntBinOp.Div => arg1 / arg2
            case IntBinOp.Lt  => if arg1 < arg2 then 1 else 0
            case IntBinOp.Eq  => if arg1 == arg2 then 1 else 0
            case IntBinOp.Gt  => if arg1 > arg2 then 1 else 0

          stack.push(VM.Value.Integer(result))

  def currentValue: Option[VM.Value] =
    stack.headOption

object VM:
  enum Value:
    case Integer(v: Int)

  object Value:
    def asInt(v: Value): Int = v match
      case Integer(v) => v
      // case other      => throw new Exception(s"Unexpected value type: expected Integer, got ${other}")
