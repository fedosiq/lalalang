package lalalang.lib.interpreters.bytecode

import cats.data.StateT
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.{Id, Monad}
import lalalang.lib.expr.*
import lalalang.lib.expr.BuiltinFn.*

type Bytecode       = List[Instr]
type BytecodeChunks = Vector[Vector[Instr]]

type BytecodeState[F[_]] = Stateful[F, State]
case class State(chunks: BytecodeChunks, pool: ConstPool, labelPool: LabelPool)

object State {
  def empty: State = State(Vector.empty[Vector[Instr]], ConstPool.empty, LabelPool.empty)
}

object Bytecode:
  def generate(expr: Expr): Bytecode =
    val bb: BytecodeBuilder[StateT[Id, State, *]] =
      BytecodeBuilder[StateT[Id, State, *]]()
    val entryChunk = bb.allocNamedChunk("entry")

    bb
      .generate(entryChunk.num, expr)
      .runS(State.empty)
      ._1
      .last
      .toList

  def eval(expr: Expr): VM.Value =
    val bc = generate(expr)
    // bc.foreach(println)
    val vm = VM(bc)
    vm.execute
    vm.currentValue.get

enum IntBinOp:
  case Add, Sub, Mul, Div, Lt, Eq, Gt

type ConstNum = Int
type ChunkNum = Int
type LabelNum = Int

case class NamedChunk(num: ChunkNum, name: LabelNum)

case class Pool(name2num: Map[String, Int], num2name: Vector[String]) {
  def appended(name: String, num: Int): Pool =
    val newNum2name = num2name.appended(name)
    val newName2num = name2num + (name -> num)
    Pool(newName2num, newNum2name)

  def add(name: String): (Int, Pool) =
    name2num.get(name) match
      case Some(num) =>
        num -> this
      case None =>
        val num = num2name.length
        num -> appended(name, num)
}

object Pool {
  def empty: Pool = Pool(Map.empty[String, Int], Vector.empty[String])
}

opaque type ConstPool = Pool
object ConstPool:
  def empty: ConstPool = Pool.empty

  def add[F[_]: Monad](name: String)(using S: Stateful[F, State]): F[ConstNum] =
    for
      pool <- S.inspect[Pool](_.pool)
      (newNum, newPool) = pool.add(name)
      _ <- S.modify(_.copy(pool = newPool))
    yield newNum
end ConstPool

opaque type LabelPool = Pool
object LabelPool:
  def empty: LabelPool = Pool.empty

  def add[F[_]: Monad](label: String)(using S: Stateful[F, State]): F[LabelNum] =
    for
      pool <- S.inspect[Pool](_.labelPool)
      (newNum, newPool) = pool.add(label)
      _ <- S.modify(_.copy(labelPool = newPool))
    yield newNum
end LabelPool

enum Instr:
  case Halt
  case IntConst(v: Int)
  case IntBinOpInstr(op: IntBinOp)
  // case IntUnaryOpInstr(op: IntBinOp)
  case EnvLoad(name: ConstNum)
  case EnvSave(name: ConstNum)
  case EnvRestore(name: ConstNum)
  case EnvUpdate(name: ConstNum)
  case MakeClosure(name: ConstNum, code: LabelNum)
  case Apply
  case Return
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
class BytecodeBuilder[F[_]: BytecodeState: Monad]:
  private def emit(chunk: ChunkNum, instr: Instr): F[Unit] =
    Stateful[F, State]
      .modify { state =>
        val existing = state.chunks.get(chunk)
        val newChunks = existing match
          case None =>
            state.chunks :+ Vector(instr)

          case Some(vec) =>
            state.chunks.updated(chunk, vec.appended(instr))

        state
          .copy(chunks = newChunks)
      }

  def allocNamedChunk(name: String): NamedChunk = NamedChunk(0, 0)

  def generate(chunkNum: ChunkNum, expr: Expr): F[Unit] =
    expr match
      case Expr.Lit(x) =>
        emit(chunkNum, Instr.IntConst(x))

      case Expr.Builtin(BuiltinFn.Arithmetic(fn, a, b)) =>
        generate(chunkNum, a)
          >> generate(chunkNum, b)
          >> emit(chunkNum, Instr.IntBinOpInstr(IntBinOp.fromArithmeticFn(fn)))

      case Expr.Builtin(BuiltinFn.Comparison(fn, a, b)) =>
        generate(chunkNum, a)
          >> generate(chunkNum, b)
          >> emit(chunkNum, Instr.IntBinOpInstr(IntBinOp.fromComparisonFn(fn)))

      case Expr.Var(name) =>
        ConstPool
          .add(name)
          .flatMap(nameNum => emit(chunkNum, Instr.EnvLoad(nameNum)))

      case Expr.Bind(Expr.Binding(rec, varName, body), inExpr) =>
        ConstPool.add(varName).flatMap { varNum =>
          if (!rec)
            generate(chunkNum, body)
              >> emit(chunkNum, Instr.EnvSave(varNum))
              >> generate(chunkNum, inExpr)
              >> emit(chunkNum, Instr.EnvRestore(varNum))
          else
            emit(chunkNum, Instr.Blackhole)
              >> emit(chunkNum, Instr.EnvSave(varNum))
              >> generate(chunkNum, body)
              >> emit(chunkNum, Instr.EnvUpdate(varNum))
              >> generate(chunkNum, inExpr)
              >> emit(chunkNum, Instr.EnvRestore(varNum))
        }

      case Expr.Abs(v, body) =>
        ConstPool.add(v).flatMap { varNum =>
          val NamedChunk(bodyChunk, bodyLabel) = allocNamedChunk("Lam")

          generate(bodyChunk, body) >>
            emit(chunkNum, Instr.Return) >>
            emit(chunkNum, Instr.MakeClosure(varNum, bodyLabel))
        }

      case Expr.App(body, arg) =>
        generate(chunkNum, arg) >>    // stack: argValue
          generate(chunkNum, expr) >> // stack: argValue closureValue
          emit(chunkNum, Instr.Apply) // stack: applicationValue

      case _ => ???

  end generate

end BytecodeBuilder
