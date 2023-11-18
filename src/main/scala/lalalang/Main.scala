package lalalang

import lalalang.lib.Show.instances.given
import lalalang.lib.*

def timed[A](a: => A): (A, Long) = {
  val start = System.currentTimeMillis
  val res   = a
  val time  = System.currentTimeMillis - start
  res -> time
}

def reduceExample(expr: Expr): Unit =
  println("inner repr:")
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

  reduceExample(identityApply(1))
  reduceExample(incApply(42))

  println(s"T = ${t.show}")
  println(s"F = ${f.show}")
  println(s"AND = ${and.show}")

  println("-" * 30)

  reduceExample(tf)
  reduceExample(tft)

  reduceExample(andtf)
  reduceExample(andt)

  // reduceExample(Expr.App(eagerFixpoint, inc))

  pprint.pprintln(fib(0, false))
  reduceExample(fib(0, false))
  reduceExample(fib(10, false))
