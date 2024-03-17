package lalalang.examples

import cats.effect.IO
import lalalang.lib.Show.instances.given
import lalalang.lib.*
import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.TreeInterpreter.Error
import lalalang.lib.interpreters.*
import lalalang.lib.interpreters.bytecode.{Bytecode, VM}
import lalalang.lib.util.timed

import scala.util.{Failure, Success, Try}
import lalalang.lib.expr.dsl.*

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

  // println(s"T = ${t.show}")
  // println(s"F = ${f.show}")
  // println(s"AND = ${and.show}")

  def expr = let("y" -> lit(11)).in {
    let("x" -> add(lit(3), lit(1)))
      .in(mul(Expr.Var("x"), Expr.Var("y")))
  }

  val expressions = List(
    twoTimesTwo,
    twoPlus3Times4,
    expr,
    identityApply(1),
    incApply(42),
    tf,
    tft,
    andtf,
    andt,
    // fib(0, _lazy = true),                                  // only subst
    // fib(20, _lazy = true),                                 // only subst
    fib(0, _lazy = false),  // only env
    fib(10, _lazy = false), // only env
    fibDirect(10),          // only env
    diverging               // only env, blackhole
  )
  val envInterpreter = EnvInterpreter[IO](debug = false)

  val interpreters = List(
    "substitutional tree interpreter" -> evalPrint[Expr](
      e => TreeInterpreter.eval[Either[Error, *]](e).toOption.get,
      debug = false
    ),
    "env tree interpreter" -> evalPrint[EnvInterpreter.Value[IO]](
      e => envInterpreter.initEval(Map.empty)(e).unsafeRunSync,
      debug = false
    ),
    "bytecode interpreter" -> evalPrint[VM.Value](Bytecode.eval, debug = false)
  )

  (for
    expr        <- expressions
    interpreter <- interpreters
  yield expr -> interpreter)
    .foreach { case (expr, (name, interpreter)) =>
      println(name)
      Try(interpreter(expr)) match {
        case Failure(err) =>
          println(s"failed: ${err.getMessage()}")
        // err.printStackTrace()
        case Success(_) =>
      }
    }

  // should not complete
  // envEvalPrint(Expr.App(eagerFixpoint, inc))
