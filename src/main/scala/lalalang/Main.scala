package lalalang

import lalalang.lib.Expr.{envEval, substitutionEval}
import lalalang.lib.Show.instances.given
import lalalang.lib.*

import scala.util.Try

def timed[A](a: => A): (A, Long) = {
  val start = System.currentTimeMillis
  val res   = a
  val time  = System.currentTimeMillis - start
  res -> time
}

def reduceExample(expr: Expr): Unit =
  println("input inner repr:")
  pprint.pprintln(expr)

  val (res, time1)        = timed(Expr.substitutionEval(expr))
  val (envEvalRes, time2) = timed(Expr.envEval(Map.empty)(expr))

  println(s"[${time1}ms] inner repr of substitution result:")
  pprint.pprintln(res)

  println(s"${expr.show} ~> ${res.show}")
  println("-" * 50 + "\n")

  println(s"[${time2}ms] inner repr of env result:")
  pprint.log(envEvalRes)
  println("-" * 50 + "\n")

def evalPrint[T: Show](evalFn: Expr => T, debug: Boolean)(expr: Expr): Unit =
  if (debug)
    println("input inner repr:")
    pprint.pprintln(expr)

  val (res, time) = timed(evalFn(expr))

  print(s"[${time}ms]: ")
  pprint.pprintln(res)

  println(s"${expr.show} ~> ${res.show}")
  println("-" * 50 + "\n")

def tree(expr: Expr, indent: Int): String =
  val spaces = "\n" + " " * indent
  expr match
    case v: Expr.Var => spaces + v.toString
    case Expr.Abs(variable, body) =>
      spaces + s"Abs(\n${" " * (indent + 2)}${variable},${tree(body, indent + 2)}$spaces)"

    case Expr.App(expr, arg) =>
      spaces + s"App(${tree(expr, indent + 2)},${tree(arg, indent + 2)}$spaces)"

    case l: Expr.Lit      => spaces + l.toString
    case Expr.Builtin(fn) => spaces + fn.toString

    case Expr.Cond(pred, trueBranch, falseBranch) =>
      spaces + s"Cond(${tree(pred, indent + 2)},${tree(trueBranch, indent + 2)},${tree(falseBranch, indent + 2)}"
    case _: Expr.Bind => ???

@main def parseTest: Unit =
  val parser = LCParser()
  println(parser.parse("λa.λb.a"))
  println(parser.parse("(λa.λb.a) 2 4"))

  val examples = List(
    "(λa.λb.a) 2 4",
    "((λt.λf.t) λt.λf.f) λt.λf.t",
    "((λp.λq.((p) q) p) λt.λf.t) λt.λf.f",
    "λf.(λx.f (x x)) λx.f (x x)"
  )

  examples.foreach(parser.parse(_).map(reduceExample))

@main def reduceTest: Unit =
  import functions.*
  import functions.booleans.*
  import Expr.dsl.*

  // println(s"T = ${t.show}")
  // println(s"F = ${f.show}")
  // println(s"AND = ${and.show}")

  val expressions = List(
    twoTimesTwo,
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

  val interpreters = List(
    "substitutional tree interpreter" -> evalPrint(substitutionEval, debug = false),
    "env tree interpreter"            -> evalPrint(envEval(Map.empty), debug = false),
    "bytecode interpreter"            -> evalPrint(Bytecode.eval, debug = false)
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
