package lalalang.lib.expr

import lalalang.lib.Show
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

  given Show[Expr] =
    case Var(name)           => name
    case Abs(variable, body) => s"λ$variable.${body.show}"
    case App(expr, arg)      => s"(${expr.show}) (${arg.show})"
    case Lit(x)              => x.toString
    case Builtin(fn)         => fn.show
    case Cond(pred, trueBranch, falseBranch) =>
      s"if (${pred.show}) then {${trueBranch.show}} else {${falseBranch.show}}"
    case Bind(Binding(rec, name, body), expr) =>
      s"${expr.show} [${if (rec) "rec " else ""}$name = ${body.show}]"
