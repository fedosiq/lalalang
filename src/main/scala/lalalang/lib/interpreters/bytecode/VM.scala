package lalalang.lib.interpreters.bytecode

import scala.collection.mutable
import lalalang.lib.util.|>

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
