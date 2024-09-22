package lalalang.lib.interpreters.bytecode

import cats.data.StateT
import cats.mtl.Stateful
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Id, Monad}
import lalalang.lib.expr.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.util.FunctorK.syntax.*
import lalalang.lib.util.given
import lalalang.lib.util.{FunctorK, Lens, ~>}

type Bytecode       = List[Instr]
type BytecodeChunks = Vector[Vector[Instr]]

case class State(chunks: BytecodeChunks, pool: ConstPool, labelPool: LabelPool, mappings: LabelManager.Mappings)

object State {
  def empty: State = State(Vector.empty[Vector[Instr]], ConstPool.empty, LabelPool.empty, LabelManager.Mappings())

  given Lens[State, ConstPool] = new:
    def get: State => ConstPool          = _.pool
    def set: ConstPool => State => State = newPool => _.copy(pool = newPool)

  given Lens[State, LabelManager.Mappings] = Lens.instance(
    _.mappings,
    newMappings => _.copy(mappings = newMappings)
  )
}

type BytecodeState[F[_]] = Stateful[F, State]

// type Eff = StateT[Id, State, *]

object Bytecode:
  def generate(expr: Expr): Bytecode =
    val labelManager: LabelManagerAlg[StateT[Id, State, *]] =
      val lm: LabelManagerAlg[StateT[Id, LabelManager.Mappings, *]] =
        LabelManager[StateT[Id, LabelManager.Mappings, *]]()
      lm.mapK

    val bb: BytecodeBuilder[StateT[Id, State, *]] =
      BytecodeBuilder[StateT[Id, State, *]](labelManager)

    bb
      .allocNamedChunk("entry")
      .flatMap(entryChunk => bb.generate(entryChunk.chunkNum, expr))
      .runS(State.empty)
      ._1
      .last
      .toList

  def eval(expr: Expr): VM.Value =
    val bc = generate(expr)
    val vm = VM(bc)
    vm.execute
    vm.currentValue.get

enum IntBinOp:
  case Add, Sub, Mul, Div, Lt, Eq, Gt

type ConstNum = Int
type ChunkNum = Int
type LabelNum = Int

case class NamedChunk(chunkNum: ChunkNum, labelNum: LabelNum)

trait LabelManagerAlg[F[_]] {
  def add(name: String): F[LabelNum]
}

object LabelManagerAlg {
  given FunctorK[LabelManagerAlg] with
    def mapK[F[_], G[_]](sf: LabelManagerAlg[F])(f: F ~> G): LabelManagerAlg[G] =
      name => f(sf.add(name))
}

class LabelManager[F[_]: LabelManager.State: Monad] extends LabelManagerAlg[F] {
  private def appended(name: String, num: Int): F[Unit] =
    Stateful[F, LabelManager.Mappings].modify { mappings =>
      val newNum2name = mappings.num2name.appended(name)
      val newName2num = mappings.name2num + (name -> num)
      mappings.copy(newName2num, newNum2name)
    }

  def add(name: String): F[LabelNum] =
    Stateful[F, LabelManager.Mappings].get.flatMap { mappings =>
      val nextNum = mappings.num2name.length
      val uniqueName =
        if mappings.name2num.contains(name)
        then s"$name$nextNum"
        else name
      appended(uniqueName, nextNum).as(nextNum)
    }
}

object LabelManager {
  type State[F[_]] = Stateful[F, Mappings]

  case class Mappings(name2num: Map[String, LabelNum] = Map.empty, num2name: Vector[String] = Vector.empty)
}

case class Pool(name2num: Map[String, Int], num2name: Vector[String]) {
  private def appended(name: String, num: Int): Pool =
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
class BytecodeBuilder[F[_]: BytecodeState: Monad](lm: LabelManagerAlg[F]):
  private def emit(chunk: ChunkNum, instr: Instr): F[Unit] =
    Stateful[F, State].modify { state =>
      val existing = state.chunks.lift(chunk)
      val newChunks = existing match
        case None =>
          state.chunks :+ Vector(instr)

        case Some(vec) =>
          state.chunks.updated(chunk, vec.appended(instr))

      state.copy(chunks = newChunks)
    }

  def allocNamedChunk(name: String): F[NamedChunk] =
    for {
      labelNum <- lm.add(name)
      chunks   <- Stateful[F, State].inspect(_.chunks)
      chunkNum = chunks.length
      _ <- Stateful[F, State].modify(
        _.copy(chunks = chunks :+ Vector())
      )
    } yield NamedChunk(chunkNum, labelNum)

  def generate(chunkNum: ChunkNum, expr: Expr): F[Unit] =
    val emitCur = emit(chunkNum, _)
    expr match
      case Expr.Lit(x) =>
        emitCur(Instr.IntConst(x))

      case Expr.Builtin(BuiltinFn.Arithmetic(fn, a, b)) =>
        generate(chunkNum, a)
          >> generate(chunkNum, b)
          >> emitCur(Instr.IntBinOpInstr(IntBinOp.fromArithmeticFn(fn)))

      case Expr.Builtin(BuiltinFn.Comparison(fn, a, b)) =>
        generate(chunkNum, a)
          >> generate(chunkNum, b)
          >> emitCur(Instr.IntBinOpInstr(IntBinOp.fromComparisonFn(fn)))

      case Expr.Var(name) =>
        ConstPool
          .add(name)
          .flatMap(nameNum => emitCur(Instr.EnvLoad(nameNum)))

      case Expr.Bind(Expr.Binding(rec, varName, body), inExpr) =>
        ConstPool.add(varName).flatMap { varNum =>
          if (!rec)
            generate(chunkNum, body)
              >> emitCur(Instr.EnvSave(varNum))
              >> generate(chunkNum, inExpr)
              >> emitCur(Instr.EnvRestore(varNum))
          else
            emitCur(Instr.Blackhole)
              >> emitCur(Instr.EnvSave(varNum))
              >> generate(chunkNum, body)
              >> emitCur(Instr.EnvUpdate(varNum))
              >> generate(chunkNum, inExpr)
              >> emitCur(Instr.EnvRestore(varNum))
        }

      case Expr.Abs(v, body) =>
        for {
          varNum                           <- ConstPool.add(v)
          NamedChunk(bodyChunk, bodyLabel) <- allocNamedChunk("Lam")
          _                                <- generate(bodyChunk, body)
          _                                <- emitCur(Instr.Return)
          _                                <- emitCur(Instr.MakeClosure(varNum, bodyLabel))
        } yield ()

      case Expr.App(body, arg) =>
        generate(chunkNum, arg) >>    // stack: [argValue]
          generate(chunkNum, expr) >> // stack: [argValue, closureValue]
          emitCur(Instr.Apply)        // stack: [applicationValue]

      case _ => ???
  end generate

end BytecodeBuilder
