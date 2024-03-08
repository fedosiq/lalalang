package lalalang.lib.interpreters.bytecode

import cats.data.{Chain, RWST}
import cats.mtl.{Stateful, Tell}
import cats.syntax.all.*
import cats.{Id, Monad}
import lalalang.lib.expr.ArithmeticFn.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.ComparisonFn.*
import lalalang.lib.expr.*

type Bytecode = List[Instr]

type TellInstr[F[_]]      = Tell[F, Chain[Instr]]
type ConstPoolState[F[_]] = Stateful[F, ConstPool]

object Bytecode:
  def generate(expr: Expr): Bytecode =
    val bb = BytecodeBuilder[RWST[Id, Unit, Chain[Instr], ConstPool, *]]()
    bb.generate(expr).runL((), ConstPool.empty).toList

  def eval(expr: Expr): VM.Value =
    val bc = generate(expr)
    val vm = VM(bc)
    vm.execute
    vm.currentValue.get

enum IntBinOp:
  case Add, Sub, Mul, Div, Lt, Eq, Gt

type ConstNum = Int

case class ConstPool(name2num: Map[String, ConstNum], num2name: Vector[String])

object ConstPool:
  def empty = ConstPool(Map.empty[String, ConstNum], Vector.empty[String])
  def findName(pool: ConstPool, num: ConstNum): String =
    pool.num2name(num)

  private def get[F[_]: ConstPoolState]: F[ConstPool] =
    Stateful[F, ConstPool].get

  private def append[F[_]: ConstPoolState](pool: ConstPool, name: String, num: ConstNum): F[Unit] =
    val newNum2name = pool.num2name.appended(name)
    val newName2num = pool.name2num + (name -> num)
    Stateful[F, ConstPool]
      .set(ConstPool(newName2num, newNum2name))

  def add[F[_]: ConstPoolState: Monad](name: String): F[ConstNum] =
    get.flatMap { pool =>
      pool.name2num.get(name) match
        case Some(num) =>
          num.pure
        case None =>
          val nextNum = pool.num2name.length
          append(pool, name, nextNum)
            .as(nextNum)
    }
end ConstPool

enum Instr:
  case Halt
  case IntConst(v: Int)
  case IntBinOpInstr(op: IntBinOp)
  // case IntUnaryOpInstr(op: IntBinOp)
  case EnvLoad(name: ConstNum)
  case EnvSave(name: ConstNum)
  case EnvRestore(name: ConstNum)
  case EnvUpdate(name: ConstNum)
  case Blackhole

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

/** Emits bytecode for stack-based VM.
  */
class BytecodeBuilder[F[_]: TellInstr: ConstPoolState: Monad]:
  private def emit(instr: Instr): F[Unit] =
    Tell[F, Chain[Instr]].tell(Chain.one(instr))

  def generate(expr: Expr): F[Unit] =
    expr match
      case Expr.Lit(x) =>
        emit(Instr.IntConst(x))

      case Expr.Builtin(BuiltinFn.Arithmetic(fn, a, b)) =>
        generate(a)
          >> generate(b)
          >> emit(Instr.IntBinOpInstr(IntBinOp.fromArithmeticFn(fn)))

      case Expr.Builtin(BuiltinFn.Comparison(fn, a, b)) =>
        generate(a)
          >> generate(b)
          >> emit(Instr.IntBinOpInstr(IntBinOp.fromComparisonFn(fn)))

      case Expr.Var(name) =>
        ConstPool
          .add(name)
          .flatMap(nameNum => emit(Instr.EnvLoad(nameNum)))

      case Expr.Bind(Expr.Binding(rec, varName, body), inExpr) =>
        ConstPool.add(varName).flatMap { varNum =>
          if (!rec)
            generate(body)
              >> emit(Instr.EnvSave(varNum))
              >> generate(inExpr)
              >> emit(Instr.EnvRestore(varNum))
          else
            emit(Instr.Blackhole)
              >> emit(Instr.EnvSave(varNum))
              >> generate(body)
              >> emit(Instr.EnvUpdate(varNum))
              >> generate(inExpr)
              >> emit(Instr.EnvRestore(varNum))
        }

      case _ => ???

  end generate

end BytecodeBuilder
