package lalalang
package lib

import model.VarName
import scala.util.chaining.*
import util.*

type VarName = String
type Env     = Map[VarName, Value]

enum Value:
  case Number(num: Int)
  case Closure(env: Env, varName: VarName, body: Expr)

object Value:
  def asInt: Value => Int =
    case Number(n) => n
    case other     => throw new Exception(s"expected a literal, got $other")

  def asClosure: Value => Value.Closure =
    case c: Closure => c
    case other      => throw new Exception(s"expected a closure, got $other")

enum Expr:
  case Var(name: VarName)
  case Abs(variable: VarName, body: Expr)
  case App(expr: Expr, arg: Expr)
  case Lit(x: Int)
  case Builtin(fn: BuiltinFn)
  case Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr)

object Expr:
  import Expr.*
  import BuiltinFn.*

  def asInt(expr: Expr): Int =
    expr match
      case Lit(a) => a
      case other  => throw new Exception(s"expected a literal, got $other")

  def substitute(target: VarName, replacement: Expr)(expr: Expr): Expr =
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

  def substitutionEval: Expr => Expr =
    case v: Var   => v
    case abs: Abs => abs
    case lit: Lit => lit

    case App(appBody, arg) => // do not eval arg here to get lazy evaluation
      substitutionEval(appBody) match
        case Abs(v, lambdaBody) => substitutionEval(substitute(v, arg)(lambdaBody))
        case other              => throw new Exception(s"Expected lambda abstraction, got $other")

    case Builtin(Arithmetic(op, a, b)) =>
      op.applyExpr(substitutionEval(a), substitutionEval(b))
    case Builtin(Comparison(op, a, b)) =>
      op.applyExpr(substitutionEval(a), substitutionEval(b))

    case Cond(pred, trueBranch, falseBranch) =>
      substitutionEval(pred) match
        case Lit(x) if x == 1 => substitutionEval(trueBranch)
        case Lit(x) if x == 0 => substitutionEval(falseBranch)
        case other            => throw new Exception(s"Expected literal integer, got $other")
  end substitutionEval

  def envEval(env: Env)(expr: Expr): Value =
    expr match
      case Var(name) =>
        env
          .get(name)
          .getOrElse(throw new Exception(s"unbound name $name"))

      case Abs(name, body) => Value.Closure(env, name, body)
      case Lit(n)          => Value.Number(n)

      case App(body, arg) => // do not eval arg here to get lazy evaluation
        def closure  = envEval(env)(body) |> Value.asClosure
        def argValue = envEval(env)(arg) // eager arg evaluation
        def newEnv   = closure.env + (closure.varName -> argValue)
        envEval(newEnv)(closure.body)

      case Builtin(Arithmetic(op, a, b)) =>
        op
          .apply(
            Value.asInt(envEval(env)(a)),
            Value.asInt(envEval(env)(b))
          )
          .pipe(Value.Number(_))

      case Builtin(Comparison(op, a, b)) =>
        op
          .apply(
            Value.asInt(envEval(env)(a)),
            Value.asInt(envEval(env)(b))
          )
          .pipe(Value.Number(_))

      case Cond(pred, trueBranch, falseBranch) =>
        envEval(env)(pred) |> Value.asInt match
          case 1 => envEval(env)(trueBranch)
          case 0 => envEval(env)(falseBranch)
  end envEval
