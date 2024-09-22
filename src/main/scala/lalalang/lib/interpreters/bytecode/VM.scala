package lalalang.lib.interpreters.bytecode

import scala.collection.mutable
import lalalang.lib.util.|>
import VM.*

/** Kind of like a stack-based VM
  */
class VM(bc: Bytecode):
  private var env: Env    = Env.empty
  private val stack       = mutable.Stack.empty[Value]
  private val envStack    = mutable.Stack.empty[Env]
  private var codePointer = 0

  def execute: Unit =
    var shouldHalt = false

    while (codePointer < bc.instr.length && !shouldHalt)
      val instr = bc.instr(codePointer)
      codePointer += 1

      instr match
        case Instr.Halt =>
          shouldHalt = true
        case Instr.IntConst(v) =>
          stack.push(Value.Integer(v))
        case Instr.IntBinOpInstr(op) =>
          val arg2 = stack.pop() |> Value.asInt
          val arg1 = stack.pop() |> Value.asInt

          val result = op match
            case IntBinOp.Add => arg1 + arg2
            case IntBinOp.Sub => arg1 - arg2
            case IntBinOp.Mul => arg1 * arg2
            case IntBinOp.Div => arg1 / arg2
            case IntBinOp.Lt  => if arg1 < arg2 then 1 else 0
            case IntBinOp.Eq  => if arg1 == arg2 then 1 else 0
            case IntBinOp.Gt  => if arg1 > arg2 then 1 else 0

          stack.push(Value.Integer(result))

        case Instr.EnvLoad(n) =>
          val value = Env.find(n)(env)
          stack.push(value)
        case Instr.EnvSave(n) =>
          val value = stack.pop()
          envStack.push(env)
          Env.add(n, value)(env)

        case Instr.EnvUpdate(n) =>
          val value = stack.pop()
          Env.add(n, value)(env)
        case Instr.EnvRestore(_) =>
          env = envStack.pop()
        case Instr.Blackhole => ???
        case _               => ???

  def currentValue: Option[Value] =
    stack.headOption

object VM:
  opaque type Env = mutable.Map[ConstNum, Value]

  object Env:
    def empty: Env = mutable.Map.empty
    def find(name: ConstNum)(env: Env): Value =
      env.getOrElse(name, throw new NoSuchElementException(s"Unbound name: ${name}"))

    def add(k: ConstNum, v: Value)(env: Env): Unit =
      env.addOne(k -> v)

  enum Value:
    case Integer(v: Int)
    case Blackhole

  object Value:
    def asInt(v: Value): Int = v match
      case Integer(v) => v
      case other      => throw new Exception(s"Unexpected value type: expected Integer, got ${other}")
