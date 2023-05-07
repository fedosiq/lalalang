package lalalang
package lib

enum Expr:
  case Var(name: String)
  case Abs(variable: String, body: Expr)
  case App(expr: Expr, arg: Expr)
  case Lit(x: Int)
  case Builtin(fn: BuiltinFn)
  case Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr)

object Expr:
  import Expr.*
  import BuiltinFn.*

  def substitute(target: String, replacement: Expr)(expr: Expr): Expr =
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

  def reduce(expr: Expr): Expr =
    expr match
      case v: Var   => v
      case abs: Abs => abs
      case lit: Lit => lit

      case App(appBody, arg) => // do not eval arg here to get lazy evaluation
        reduce(appBody) match
          case Abs(v, lambdaBody) => reduce(substitute(v, arg)(lambdaBody))
          case _                  => throw new Exception("Should not happen")

      case Builtin(Arithmetic(op, a, b)) => op.apply(reduce(a), reduce(b))
      case Builtin(Comparison(op, a, b)) => op.apply(reduce(a), reduce(b))

      case Cond(pred, trueBranch, falseBranch) =>
        reduce(pred) match
          case Lit(x) if x == 1 => reduce(trueBranch)
          case Lit(x) if x == 0 => reduce(falseBranch)
          case _                => throw new Exception("Should not happen")
