package lalalang.lib.interpreters.bytecode

import cats.Monad
import cats.data.StateT
import cats.mtl.Stateful
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lalalang.lib.expr.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.util.FunctorK.syntax.*
import lalalang.lib.util.given
import lalalang.lib.util.{FunctorK, Id, Lens, ~>}

type BytecodeChunks = Vector[Vector[Instr]]

case class State(
    chunks: BytecodeChunks,
    constPool: ConstPool,
    labelPool: LabelPool,
    mappings: LabelManager.Mappings,
    labelMapping: Map[LabelNum, ChunkNum]
)

object State {
  def empty: State =
    State(Vector.empty[Vector[Instr]], ConstPool.empty, LabelPool.empty, LabelManager.Mappings(), Map.empty)

  given Lens[State, ConstPool] = Lens.instance(
    _.constPool,
    newPool => _.copy(constPool = newPool)
  )

  given Lens[State, LabelManager.Mappings] = Lens.instance(
    _.mappings,
    newMappings => _.copy(mappings = newMappings)
  )
}

type BytecodeState[F[_]] = Stateful[F, State]

case class Bytecode(instr: Vector[Instr], labelOffsets: Vector[Int])

// type Eff = StateT[Id, State, *]
object Bytecode:
  def generate(expr: Expr): Bytecode =
    val bb = BytecodeBuilder.make[StateT[Id, State, *], StateT[Id, LabelManager.Mappings, *]]
    val bc =
      for
        NamedChunk(entryChunk, _) <- bb.allocNamedChunk("entry")
        _                         <- bb.generate(entryChunk, expr)
        _                         <- bb.emit(entryChunk, Instr.Halt)
        bc                        <- bb.build
      yield bc

    bc.runA(State.empty)

  def eval(expr: Expr): VM.Value =
    val bc = generate(expr)
    val vm = VM(bc)
    vm.execute
    vm.currentValue.get

type ConstNum = Int
type ChunkNum = Int
type LabelNum = Int

case class NamedChunk(chunkNum: ChunkNum, labelNum: LabelNum)

// todo: Maybe introduce like a bidirectional dictionary
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
      pool <- S.inspect[Pool](_.constPool)
      (newNum, newPool) = pool.add(name)
      _ <- S.modify(_.copy(constPool = newPool))
    yield newNum
end ConstPool

// todo: remove? seems unneeded
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

/** Emits bytecode for stack-based VM.
  */
class BytecodeBuilder[F[_]: BytecodeState: Monad](labelManager: LabelManagerAlg[F]):
  def emit(chunk: ChunkNum, instr: Instr): F[Unit] =
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
    for
      labelNum <- labelManager.add(name)
      chunks   <- Stateful[F, State].inspect(_.chunks)
      chunkNum = chunks.length
      _ <- Stateful[F, State].modify(s =>
        s.copy(
          chunks = chunks :+ Vector(),
          labelMapping = s.labelMapping.updated(labelNum, chunkNum)
        )
      )
    yield NamedChunk(chunkNum, labelNum)

  // def entryChunk =

  def build: F[Bytecode] =
    Stateful[F, State].get.map { state =>
      val flatBytecode = state.chunks.flatten

      val (_, chunkOffsets) = state.chunks
        .foldLeft((0, Vector.empty[Int])) { case ((currentOffset, chunkOffsets), chunk) =>
          (currentOffset + chunk.length, chunkOffsets.appended(currentOffset))
        }

      val offsets = state.labelMapping.valuesIterator
        .map(chunkOffsets)
        .toVector

      Bytecode(flatBytecode, offsets)
    }
  end build

  def generate(chunkNum: ChunkNum, expr: Expr): F[Unit] =
    val emitCur     = emit(chunkNum, _)
    val generateCur = generate(chunkNum, _)

    expr match
      case Expr.Lit(x) =>
        emitCur(Instr.IntConst(x))

      case Expr.Builtin(BuiltinFn.Arithmetic(fn, a, b)) =>
        generateCur(a)
          >> generateCur(b)
          >> emitCur(Instr.IntBinOpInstr(IntBinOp.fromArithmeticFn(fn)))

      case Expr.Builtin(BuiltinFn.Comparison(fn, a, b)) =>
        generateCur(a)
          >> generateCur(b)
          >> emitCur(Instr.IntBinOpInstr(IntBinOp.fromComparisonFn(fn)))

      case Expr.Var(name) =>
        ConstPool
          .add(name)
          .flatMap(nameNum => emitCur(Instr.EnvLoad(nameNum)))

      case Expr.Bind(Expr.Binding(rec, varName, body), inExpr) =>
        ConstPool.add(varName).flatMap { varNum =>
          if (!rec)
            generateCur(body)
              >> emitCur(Instr.EnvSave(varNum))
              >> generateCur(inExpr)
              >> emitCur(Instr.EnvRestore(varNum))
          else
            emitCur(Instr.Blackhole)
              >> emitCur(Instr.EnvSave(varNum))
              >> generateCur(body)
              >> emitCur(Instr.EnvUpdate(varNum))
              >> generateCur(inExpr)
              >> emitCur(Instr.EnvRestore(varNum))
        }

      case Expr.Abs(v, body) =>
        for
          NamedChunk(bodyChunk, bodyLabel) <- allocNamedChunk("Lam")
          _                                <- generate(bodyChunk, body)
          _                                <- emit(bodyChunk, Instr.Return)
          varNum                           <- ConstPool.add(v)
          _                                <- emitCur(Instr.MakeClosure(varNum, bodyLabel))
        yield ()

      case Expr.App(body, arg) =>
        generateCur(arg) >>    // stack: [argValue]
          generateCur(body) >> // stack: [argValue, closureValue]
          emitCur(Instr.Apply) // stack: [applicationValue]

      case Expr.Cond(pred, trueBranch, falseBranch) => ???
  end generate

end BytecodeBuilder

object BytecodeBuilder:
  case class BuildState()
  def make[F[_]: BytecodeState: Monad, G[_]: LabelManager.State: Monad](using G ~> F): BytecodeBuilder[F] =
    val lm: LabelManagerAlg[G] = LabelManager[G]()
    BytecodeBuilder(lm.mapK[F])
