package lalalang.lib

import lalalang.lib.interpreters.bytecode.VM.{Value as VMValue}
import lalalang.lib.expr.{ArithmeticFn, BuiltinFn, ComparisonFn, Expr}
import lalalang.lib.interpreters.EnvInterpreter.Value
import lalalang.lib.interpreters.EnvInterpreter

trait Show[-T]:
  extension (t: T) def show: String

object Show:
  import Expr.*
  import ArithmeticFn.*
  import ComparisonFn.*
  import BuiltinFn.*

  def apply[T: Show]: Show[T] = summon

  def instance[T](showFn: T => String): Show[T] = showFn(_)

  object instances:
    given Show[ArithmeticFn] = instance {
      case Add => "+"
      case Sub => "-"
      case Mul => "*"
      case Div => "/"
    }

    given Show[ComparisonFn] = instance {
      case Lt => "<"
      case Eq => "=="
      case Gt => ">"
    }

    given Show[BuiltinFn] = instance {
      case Arithmetic(op, a, b) => s"${a.show} ${op.show} ${b.show}"
      case Comparison(op, a, b) => s"${a.show} ${op.show} ${b.show}"
    }

    given Show[Expr] = instance {
      case Var(name)           => name
      case Abs(variable, body) => s"λ$variable.${body.show}"
      case App(expr, arg)      => s"(${expr.show}) (${arg.show})"
      case Lit(x)              => x.toString
      case Builtin(fn)         => fn.show
      case Cond(pred, trueBranch, falseBranch) =>
        s"if (${pred.show}) then {${trueBranch.show}} else {${falseBranch.show}}"
      case Bind(Binding(rec, name, body), expr) => s"${expr.show} [${if (rec) "rec" else ""} $name = ${body.show}]"
    }

    given showValue[F[_]]: Show[Value[F]] = instance {
      case Value.Closure(env, _, body) => s"Closure(<env>, ${body.show})"
      case Value.Number(num)           => s"Number($num)"
      case Value.BlackHole()           => "Blackhole"
    }

    given showEnv[F[_]]: Show[EnvInterpreter.Env[F]] =
      instance(_.map((k, v) => k -> v.show).toString)

    given Show[VMValue] = instance(_.toString)
  end instances
end Show
