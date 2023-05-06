package lalalang

import lib.*
import lib.Show.instances.given

def reduceExample(expr: Expr): Unit =
  println(s"inner repr: ${expr}")
  val res = Expr.reduce(expr)
  println(s"inner repr of result: $res")
  println(s"${expr.show} ~> ${res.show}")
  println("-" * 30)

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
