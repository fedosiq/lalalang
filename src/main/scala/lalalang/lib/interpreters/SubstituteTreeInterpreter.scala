package lalalang.lib.interpreters

import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.model.VarName

object SubstituteTreeInterpreter:
  def eval: Expr => Expr =
    case v: Var   => v
    case abs: Abs => abs
    case lit: Lit => lit
    case App(appBody, arg) => // do not eval arg here to get lazy evaluation
      eval(appBody) match
        case Abs(v, lambdaBody) => eval(substitute(v, arg)(lambdaBody))
        case other              => throw new Exception(s"Expected lambda abstraction, got $other")

    case Builtin(Arithmetic(op, a, b)) =>
      op.applyExpr(eval(a), eval(b))
    case Builtin(Comparison(op, a, b)) =>
      op.applyExpr(eval(a), eval(b))

    case Cond(pred, trueBranch, falseBranch) =>
      eval(pred) match
        case Lit(x) if x == 1 => eval(trueBranch)
        case Lit(x) if x == 0 => eval(falseBranch)
        case other            => throw new Exception(s"Expected literal integer, got $other")

    case _: Bind => throw new Exception("binding not supported in substitution based evaluation")
  end eval

  private def substitute(target: VarName, replacement: Expr)(expr: Expr): Expr =
    val subst = substitute(target, replacement)

    expr match
      case Lit(_)         => expr
      case App(body, arg) => App(subst(body), subst(arg))

      case Builtin(Arithmetic(f, a, b)) =>
        Builtin(Arithmetic(f, subst(a), subst(b)))
      case Builtin(Comparison(f, a, b)) =>
        Builtin(Comparison(f, subst(a), subst(b)))

      case Var(name) =>
        if (name == target) replacement
        else expr

      case Abs(boundName, body) =>
        if (boundName == target) expr
        else Abs(boundName, subst(body))

      case Cond(pred, trueBranch, falseBranch) =>
        Cond(subst(pred), subst(trueBranch), subst(falseBranch))

      case _: Bind => throw new Exception("binding not supported in substitution based evaluation")
  end substitute

end SubstituteTreeInterpreter
