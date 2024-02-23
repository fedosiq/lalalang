package lalalang.examples

import lalalang.lib.Show.instances.given
import lalalang.lib.*
import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.*
import lalalang.lib.interpreters.bytecode.VM
import lalalang.lib.util.timed
import lalalang.lib.interpreters.TreeInterpreter.Error

import scala.util.Try
import cats.effect.concurrent.Ref
import cats.effect.IO
import lalalang.lib.expr.model.VarName

def evalPrint[T: Show](evalFn: Expr => T, debug: Boolean)(expr: Expr): Unit =
  if (debug)
    println("input inner repr:")
    pprint.pprintln(expr)

  val (res, time) = timed(evalFn(expr))

  print(s"[${time}ms]: ")
  pprint.pprintln(res)

  println(s"${expr.show} ~> ${res.show}")
  println("-" * 50 + "\n")

@main def reduceTest: Unit =
  import functions.*
  import functions.bool.*
  import expr.dsl.*

  // println(s"T = ${t.show}")
  // println(s"F = ${f.show}")
  // println(s"AND = ${and.show}")

  val expressions = List(
    twoTimesTwo,
    twoTimes3Plus4,
    identityApply(1),
    incApply(42),
    tf,
    tft,
    andtf,
    andt,
    // fib(0, _lazy = true),                                  // only subst
    // fib(20, _lazy = true),                                 // only subst
    fib(0, _lazy = false),                                 // only env
    fib(10, _lazy = false),                                // only env
    fibDirect(10),                                         // only env
    rec("x", add(Expr.Var("x"), lit(1))).in(Expr.Var("x")) // only env, blackhole // todo: add test
  )
  val envInterpreter = EnvInterpreter[IO](debug = false)
  val env            = Ref.unsafe[IO, Map[VarName, EnvInterpreter.Value[IO]]](Map.empty)

  val interpreters = List(
    "substitutional tree interpreter" -> evalPrint[Expr](
      e => TreeInterpreter.eval[Either[Error, *]](e).toOption.get,
      debug = false
    ),
    "env tree interpreter" -> evalPrint[EnvInterpreter.Value[IO]](
      e => envInterpreter.eval(env)(e).unsafeRunSync,
      debug = false
    ),
    "bytecode interpreter" -> evalPrint[VM.Value](bytecode.eval, debug = false)
  )

  (for
    expr        <- expressions
    interpreter <- interpreters
  yield expr -> interpreter)
    .foreach { case (expr, (name, interpreter)) =>
      println(name)
      Try(interpreter(expr)).getOrElse(println("failed"))
    }

  // should not complete
  // envEvalPrint(Expr.App(eagerFixpoint, inc))
