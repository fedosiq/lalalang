package lalalang.lib

import lalalang.lib.interpreters.bytecode.VM.{Value as VMValue}
import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.EnvInterpreter.Value
import lalalang.lib.interpreters.EnvInterpreter

trait Show[-T]:
  extension (t: T) def show: String

object Show:
  def apply[T: Show]: Show[T] = summon

  object instances:
    given showValue[F[_]]: Show[Value[F]] =
      case Value.Closure(env, _, body) => s"Closure(<env>, ${body.show})"
      case Value.Number(num)           => s"Number($num)"
      case Value.BlackHole()           => "Blackhole"

    given showEnv[F[_]]: Show[EnvInterpreter.Env[F]] =
      _.map((k, v) => k -> v.show).toString

    given Show[VMValue] = _.toString

  end instances
end Show
