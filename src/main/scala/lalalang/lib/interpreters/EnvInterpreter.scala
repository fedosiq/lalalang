package lalalang.lib.interpreters

import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.model.VarName
import lalalang.lib.util.|>

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

object EnvInterpreter:
  def eval(env: Env)(expr: Expr): Value =
    val ev = eval(env)
    expr match
      case Var(name) =>
        env
          .getOrElse(name, throw new Exception(s"unbound name $name"))
          |> Value.checkBlackHole(name)

      case Abs(name, body) => Value.Closure(env, name, body)
      case Lit(n)          => Value.Number(n)

      case App(body, arg) =>
        def closure  = ev(body) |> Value.asClosure
        def argValue = ev(arg) // eager arg evaluation
        def newEnv   = closure.env + (closure.varName -> argValue)
        eval(newEnv)(closure.body)

      case Bind(Binding(rec, name, body), expr) =>
        val bodyEnv = if (!rec) env else env + (name -> Value.BlackHole)

        val bodyVal = eval(bodyEnv)(body) match
          case closure: Value.Closure =>
            val patchedClosureEnv = env + (name -> closure)
            closure.env = patchedClosureEnv
            closure
          // closure.copy(env = patchedClosureEnv)

          case other => other

        val exprEnv = env + (name -> bodyVal)
        eval(exprEnv)(expr)

      case Builtin(Arithmetic(op, a, b)) =>
        op(
          Value.asInt(ev(a)),
          Value.asInt(ev(b))
        )
          .pipe(Value.Number(_))

      case Builtin(Comparison(op, a, b)) =>
        op(
          Value.asInt(ev(a)),
          Value.asInt(ev(b))
        )
          .pipe(Value.Number(_))

      case Cond(pred, trueBranch, falseBranch) =>
        ev(pred) |> Value.asInt match
          case 1 => ev(trueBranch)
          case 0 => ev(falseBranch)
  end eval
