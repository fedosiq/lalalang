package lalalang.lib.interpreters.bytecode

import lalalang.lib.interpreters.bytecode.VM.*
import lalalang.lib.util.|>

import scala.collection.mutable

/** Kind of like a stack-based VM
  */
class VM(bc: Bytecode):
  private var env         = Env.empty
  private val stack       = mutable.Stack.empty[Value]
  private val envStack    = mutable.Stack.empty[Env]
  private val returnStack = mutable.Stack.empty[Int]
  private var codePointer = 0

  def execute: Unit =
    var shouldHalt   = false
    val instr        = bc.instr
    val labelOffsets = bc.labelOffsets

    while (codePointer < instr.length && !shouldHalt)
      val curInstr = instr(codePointer)
      codePointer += 1

      curInstr match
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
          env = Env.add(n, value)(env)

        case Instr.EnvUpdate(n) =>
          val value = stack.pop()
          env = Env.add(n, value)(env)

        case Instr.EnvRestore(_) =>
          env = envStack.pop()

        case Instr.MakeClosure(name, code) =>
          val closure = Value.Closure(env, name, code)
          stack.push(closure)

        case Instr.Apply =>
          val closure = stack.pop() |> Value.asClosure
          val arg     = stack.pop()

          envStack.push(env)
          val closureEnv = Env.add(closure.varName, arg)(closure.env)
          env = closureEnv

          returnStack.push(codePointer)
          codePointer = labelOffsets(closure.codePointer)

        case Instr.Return =>
          env = envStack.pop()
          codePointer = returnStack.pop()

        case Instr.Blackhole => ???

  def currentValue: Option[Value] =
    stack.headOption

object VM:
  opaque type Env = Map[ConstNum, Value]
  object Env:
    def empty: Env = Map.empty
    def find(name: ConstNum)(env: Env): Value =
      env.getOrElse(name, throw new NoSuchElementException(s"Unbound name: ${name}"))

    def add(k: ConstNum, v: Value)(env: Env): Env =
      env.updated(k, v)

  enum Value:
    case Integer(v: Int)
    case Closure(env: Env, varName: ConstNum, codePointer: LabelNum)
    case Blackhole

  object Value:
    def asInt(v: Value): Int = v match
      case Integer(v) => v
      case other      => throw UnexpectedValueType("Integer", other)

    def asClosure(v: Value): Value.Closure = v match
      case c: Closure => c
      case other      => throw UnexpectedValueType("Closure", other)

  class UnexpectedValueType(expected: String, actual: Value)
      extends Exception(s"Unexpected value type: expected ${expected}, got ${actual}")
