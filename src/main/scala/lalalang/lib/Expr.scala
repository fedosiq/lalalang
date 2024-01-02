package lalalang
package lib

import lalalang.lib.model.VarName
import lalalang.lib.util.*

import scala.util.chaining.*

type Env = Map[VarName, Value]

enum Value:
  case Number(num: Int)
  case Closure(var env: Env, varName: VarName, body: Expr)
  case BlackHole

object Value:
  def asInt: Value => Int =
    case Number(n) => n
    case other     => throw new Exception(s"expected a literal, got $other")

  def asClosure: Value => Value.Closure =
    case c: Closure => c
    case other      => throw new Exception(s"expected a closure, got $other")

  def checkBlackHole(name: VarName): Value => Value =
    case Value.BlackHole => throw new Exception(s"Unevaluated $name: BlackHole")
    case other           => other

enum Expr:
  case Var(name: VarName)
  case Abs(variable: VarName, body: Expr)
  case App(expr: Expr, arg: Expr)
  case Lit(x: Int)
  case Builtin(fn: BuiltinFn)
  case Cond(pred: Expr, trueBranch: Expr, falseBranch: Expr)
  case Bind(binding: Expr.Binding, expr: Expr)

object Expr:
  import Expr.*
  import BuiltinFn.*

  case class Binding(recursive: Boolean, name: VarName, bindingBody: Expr)

  object dsl:
    type Fn2 = (Expr, Expr) => Expr

    private def mkArithmetic(fn: ArithmeticFn)(a: Expr, b: Expr): Expr =
      Builtin(Arithmetic(fn, a, b))

    private def mkComparison(fn: ComparisonFn)(a: Expr, b: Expr): Expr =
      Builtin(Comparison(fn, a, b))

    val add: Fn2 = mkArithmetic(ArithmeticFn.Add)
    val sub: Fn2 = mkArithmetic(ArithmeticFn.Sub)
    val mul: Fn2 = mkArithmetic(ArithmeticFn.Mul)
    val div: Fn2 = mkArithmetic(ArithmeticFn.Div)

    val lt = mkComparison(ComparisonFn.Lt)
    val gt = mkComparison(ComparisonFn.Gt)
    val eq = mkComparison(ComparisonFn.Eq)

    def rec(name: VarName, bindingBody: Expr) =
      Binding(recursive = true, name, bindingBody)

    extension (binding: Binding)
      def in(expr: Expr): Expr =
        Bind(binding, expr)

    extension (expr: Expr)
      def where(binding: Binding): Expr =
        Bind(binding, expr)

  end dsl

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

      case _: Bind => throw new Exception("binding not supported in substitution based evaluation")
  end substitute

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

    case _: Bind => throw new Exception("binding not supported in substitution based evaluation")
  end substitutionEval

  def envEval(env: Env)(expr: Expr): Value =
    val eval = envEval(env)
    expr match
      case Var(name) =>
        env
          .getOrElse(name, throw new Exception(s"unbound name $name"))
          |> Value.checkBlackHole(name)

      case Abs(name, body) => Value.Closure(env, name, body)
      case Lit(n)          => Value.Number(n)

      case App(body, arg) =>
        def closure  = eval(body) |> Value.asClosure
        def argValue = eval(arg) // eager arg evaluation
        def newEnv   = closure.env + (closure.varName -> argValue)
        envEval(newEnv)(closure.body)

      case Bind(Binding(rec, name, body), expr) =>
        val bodyEnv = if (!rec) env else env + (name -> Value.BlackHole)

        val bodyVal = envEval(bodyEnv)(body) match
          case closure: Value.Closure =>
            val patchedClosureEnv = env + (name -> closure)
            closure.env = patchedClosureEnv
            closure
          // closure.copy(env = patchedClosureEnv)

          case other => other

        val exprEnv = env + (name -> bodyVal)
        envEval(exprEnv)(expr)

      case Builtin(Arithmetic(op, a, b)) =>
        op(
          Value.asInt(eval(a)),
          Value.asInt(eval(b))
        )
          .pipe(Value.Number(_))

      case Builtin(Comparison(op, a, b)) =>
        op(
          Value.asInt(eval(a)),
          Value.asInt(eval(b))
        )
          .pipe(Value.Number(_))

      case Cond(pred, trueBranch, falseBranch) =>
        eval(pred) |> Value.asInt match
          case 1 => eval(trueBranch)
          case 0 => eval(falseBranch)
  end envEval
