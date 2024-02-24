package lalalang.lib.interpreters

import cats.Monad
import cats.syntax.all.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.model.VarName
import tofu.syntax.raise.*

/** Interprets expression by recursively substituting variables in the AST
  */
object TreeInterpreter:
  def eval[F[_]: Error.Raise: Monad](expr: Expr): F[Expr] = expr match
    case v: Var   => v.pure
    case abs: Abs => abs.pure
    case lit: Lit => lit.pure
    case App(appBody, arg) => // do not eval arg here to get lazy evaluation
      eval(appBody).flatMap {
        case Abs(v, lambdaBody) => substitute(v, arg)(lambdaBody) >>= eval
        case other              => Error.UnexpectedOp(other.toString, "lambda abstraction").raise
      }

    case Builtin(Arithmetic(op, a, b)) =>
      (eval(a), eval(b)).mapN(op.applyExpr)
    case Builtin(Comparison(op, a, b)) =>
      (eval(a), eval(b)).mapN(op.applyExpr)

    case Cond(pred, trueBranch, falseBranch) =>
      eval(pred).flatMap {
        case Lit(1) => eval(trueBranch)
        case Lit(0) => eval(falseBranch)
        case other  => Error.UnexpectedOp(other.toString, "literal integer").raise
      }

    case _: Bind => Error.UnsupportedOp("binding").raise
  end eval

  private def substitute[F[_]: Error.Raise: Monad](target: VarName, replacement: Expr)(expr: Expr): F[Expr] =
    val subst = substitute(target, replacement)

    expr match
      case Lit(_) => expr.pure
      case App(body, arg) =>
        (subst(body), subst(arg)).mapN(App(_, _))

      case Builtin(Arithmetic(f, a, b)) =>
        (subst(a), subst(b)).mapN((sa, sb) => Builtin(Arithmetic(f, sa, sb)))

      case Builtin(Comparison(f, a, b)) =>
        (subst(a), subst(b)).mapN((sa, sb) => Builtin(Comparison(f, sa, sb)))

      case Var(name) =>
        if (name == target) replacement.pure
        else expr.pure

      case Abs(boundName, body) =>
        if (boundName == target) expr.pure
        else subst(body).map(Abs(boundName, _))

      case Cond(pred, trueBranch, falseBranch) =>
        (subst(pred), subst(trueBranch), subst(falseBranch)).mapN(Cond(_, _, _))

      case _: Bind => Error.UnsupportedOp("binding").raise
  end substitute

  enum Error(message: String) extends Exception(message):
    case UnsupportedOp(op: String) extends Error(s"${op} not supported in substitution based evaluation")
    case UnexpectedOp(received: String, expected: String) extends Error(s"Expected ${expected}, got $received")

  object Error:
    type Raise[F[_]] = tofu.Raise[F, Error]

end TreeInterpreter
