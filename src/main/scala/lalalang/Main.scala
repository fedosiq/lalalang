package lalalang

import lib.*
import lib.Show.instances.given

def mkExample(expr: Expr): Unit =
  println(s"inner repr: ${expr}")
  val res = Expr.reduce(expr)
  println(s"${expr.show} ~> ${res.show}")
  println("-" * 30)

@main def main: Unit =
  import functions.*
  mkExample(identityApply(1))
  mkExample(incApply(42))

  println(t.show)
  println(f.show)
  println(and.show)

  println("-" * 30)

  mkExample(tf)
  mkExample(tft)

  mkExample(andtf)
  mkExample(andt)
