package lalalang
package lib

enum Expr:
  case Var(name: String)
  case Abs(variable: String, body: Expr)
  case App(expr: Expr, arg: Expr) // TODO: make expr: Expr.Abs?
  case Lit(x: Int)
  case Builtin(fn: BuiltinFn)
  // case Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr)

object Expr:
  import Expr.*
  import BuiltinFn.*
  import Show.instances.given

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

      case Abs(v, body) =>
        if (v == target) expr
        else Abs(v, subst(body))

  def reduce(expr: Expr): Expr =
    expr match
      case v: Var   => v
      case abs: Abs => abs
      case lit: Lit => lit

      case App(appBody, arg) =>
        reduce(appBody) match
          case Abs(v, lambdaBody) => reduce(substitute(v, arg)(lambdaBody))
          case _                  => ???

      case Builtin(fn) => BuiltinFn.reduce(fn)
      // case Cond(pred, trueBranch, falseBranch) => ???
