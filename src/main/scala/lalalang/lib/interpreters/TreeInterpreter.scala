package lalalang.lib.interpreters

import cats.syntax.all.*
import cats.Monad
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.model.VarName
import tofu.syntax.raise.*
import scala.annotation.tailrec

/** Interprets expression by recursively substituting variables in the AST
  */
class TreeInterpreter[F[_]: TreeInterpreter.Error.Raise: Monad]:
  import TreeInterpreter.Error

  def eval(expr: Expr): F[Expr] = expr match
    case v: Var   => v.pure
    case abs: Abs => abs.pure
    case lit: Lit => lit.pure
    case App(appBody, arg) => // do not eval arg here to get lazy evaluation
      eval(appBody).flatMap {
        case Abs(v, lambdaBody) => substitute(v, arg)(lambdaBody) >>= eval
        case other              => Error.UnexpectedOp(other, "lambda abstraction").raise
      }

    case Builtin(Arithmetic(op, a, b)) =>
      (eval(a), eval(b)).mapN(op.applyExpr)
    case Builtin(Comparison(op, a, b)) =>
      (eval(a), eval(b)).mapN(op.applyExpr)

    case Cond(pred, trueBranch, falseBranch) =>
      eval(pred).flatMap {
        case Lit(1) => eval(trueBranch)
        case Lit(0) => eval(falseBranch)
        case other  => Error.UnexpectedOp(other, "literal integer (1 or 0)").raise
      }

    case _: Bind => Error.UnsupportedOp("binding").raise
  end eval

  /** [[eval]] can't simplify expressions with top-level [[Expr.Abs]].
    *
    * This function traverses the expression tree and tries to simplify it where possible.
    *
    * TODO: tests
    *
    * TODO: check if expression is beta-normalized
    */
  def reduce(expr: Expr): F[Expr] = expr match
    case App(appBody, arg) =>
      reduce(appBody).flatMap {
        case Abs(v, lambdaBody) => substitute(v, arg)(lambdaBody) >>= reduce
        case v: Var             => reduce(arg).map(App(v, _))
        case other              => reduce(other)
      }

    case Abs(varName, body) =>
      reduce(body).map(Abs(varName, _))

    case other =>
      // eval(other)
      other.pure

  def substitute(target: VarName, replacement: Expr)(expr: Expr): F[Expr] =
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

  /** Performs alpha-conversion.
    *
    * Difference from [[substitute]] is that [[substitute]] doesn't rename bound variables in Abs
    *
    * fixme: non-tailrec
    *
    * TODO: tests
    */
  def alpha(expr: Expr, rename: Map[String, String]): Expr =
    val _alpha                  = alpha(_, rename)
    def tryRename(name: String) = rename.getOrElse(name, name)

    expr match
      case Var(name)                           => Var(tryRename(name))
      case Abs(variable, body)                 => Abs(tryRename(variable), _alpha(body))
      case App(expr, arg)                      => App(_alpha(expr), _alpha(arg))
      case Lit(x)                              => Lit(x)
      case Builtin(Arithmetic(f, a, b))        => Builtin(Arithmetic(f, _alpha(a), _alpha(b)))
      case Builtin(Comparison(f, a, b))        => Builtin(Comparison(f, _alpha(a), _alpha(b)))
      case Cond(pred, trueBranch, falseBranch) => Cond(_alpha(pred), _alpha(trueBranch), _alpha(falseBranch))
      case Bind(binding, expr)                 => ???

  def tryPreprocess(expr: Expr, constants: Map[VarName, Expr]): F[Expr] =
    val freeVars = findFreeVars(expr, constants.keySet)
    freeVars match
      case Nil => expr.pure
      case Expr.Var(name) :: _ =>
        substitute(name, constants(name))(expr)
          >>= (tryPreprocess(_, constants))

  def findFreeVars(expr: Expr, constants: Set[VarName], acc: List[Expr.Var] = List.empty): List[Expr.Var] =
    def _find = findFreeVars(_, constants, acc)

    expr match
      case Expr.Lit(x) => acc
      case v @ Expr.Var(name) =>
        if (constants.contains(name))
          v :: acc
        else
          acc
      case Expr.Abs(boundName, body) =>
        if (constants.contains(boundName))
          findFreeVars(body, constants - boundName, acc)
        else
          _find(body)
      case Expr.App(body, arg) =>
        _find(body) ::: _find(arg)
      case Expr.Builtin(Arithmetic(f, a, b)) =>
        _find(a) ::: _find(b)
      case Expr.Builtin(Comparison(f, a, b)) =>
        _find(a) ::: _find(b)
      case Expr.Cond(pred, trueBranch, falseBranch) =>
        _find(pred) ::: _find(trueBranch) ::: _find(falseBranch)
      case Expr.Bind(binding, expr) => ???


end TreeInterpreter

object TreeInterpreter:
  enum Error(message: String) extends Exception(message):
    case UnsupportedOp(op: String) extends Error(s"${op} not supported in substitution based evaluation")
    case UnexpectedOp(received: Expr, expected: String) extends Error(s"Expected ${expected}, got $received")

  object Error:
    type Raise[F[_]] = tofu.Raise[F, Error]

end TreeInterpreter
