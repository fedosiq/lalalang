package lalalang

import lib.*
import lib.Show.instances.given

def reduceExample(expr: Expr): Unit =
  println(s"inner repr: ${tree(expr, 0)}")
  val res = Expr.eval(expr)
  println(s"inner repr of result: ${tree(res, 0)}")
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

  // reduceExample(Expr.App(Y, inc))

  reduceExample(fib(10))
