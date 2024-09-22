package lalalang.lib.interpreters.bytecode

import lalalang.lib.expr.{ArithmeticFn, ComparisonFn}

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
