package lalalang.lib.interpreters.bytecode

import cats.Monad
import cats.data.{Chain, Writer}
import cats.mtl.Tell
import cats.syntax.all.*
import lalalang.lib.expr.ArithmeticFn.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.ComparisonFn.*
import lalalang.lib.expr.*

type Bytecode = List[Instr]

type TellInstr[F[_]] = Tell[F, Chain[Instr]]

object Bytecode:
  def generate(expr: Expr): Bytecode =
    val bb = BytecodeBuilder[Writer[Chain[Instr], *]]()
    bb.generate(expr).run._1.toList

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

class BytecodeBuilder[F[_]: TellInstr: Monad]():
  private def emit(instr: Instr): F[Unit] =
    Tell[F, Chain[Instr]].tell(Chain.one(instr))

  def generate(expr: Expr): F[Unit] =
    expr match
      case Expr.Lit(x) =>
        emit(Instr.IntConst(x))

      case Expr.Builtin(BuiltinFn.Arithmetic(fn, a, b)) =>
        generate(a) >>
          generate(b) >>
          emit(Instr.IntBinOpInstr(IntBinOp.fromArithmeticFn(fn)))

      case Expr.Builtin(BuiltinFn.Comparison(fn, a, b)) =>
        generate(a) >>
          generate(b) >>
          emit(Instr.IntBinOpInstr(IntBinOp.fromComparisonFn(fn)))

      case _ => ???

  end generate

end BytecodeBuilder
