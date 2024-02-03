package lalalang.lib.expr

import lalalang.lib.expr.model.VarName

enum Expr:
  case Var(name: VarName)
  case Abs(variable: VarName, body: Expr)
  case App(expr: Expr, arg: Expr)
  case Lit(x: Int)
  case Builtin(fn: BuiltinFn)
  case Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr)
  case Bind(binding: Expr.Binding, expr: Expr)

object Expr:
  case class Binding(recursive: Boolean, name: VarName, bindingBody: Expr)

  def asInt(expr: Expr): Int =
    expr match
      case Lit(a) => a
      case other  => throw new Exception(s"expected a literal, got $other")
