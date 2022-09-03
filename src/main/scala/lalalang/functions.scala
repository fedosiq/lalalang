package lalalang
package functions

import lib.*
import lib.Show.instances.given

import Expr.*, ArithmeticFn.*, BuiltinFn.*

def lit: Int => Expr = Lit(_)

def inc: Expr = Abs(
  variable = "x",
  body = Builtin(Arithmetic(Add, Var("x"), Lit(1)))
)

def incApply(n: Int): Expr = App(inc, Lit(n))

def identityApply(n: Int): Expr =
  App(
    Abs(
      variable = "x",
      body = Var("x")
    ),
    Lit(n)
  )

private def lambda2(variables: (String, String), body: Expr) = {
  val (a, b) = variables
  Abs(a, Abs(b, body))
}

def Y = {
  val xx = App(Var("x"), Var("x"))

  val inner =
    Abs(
      variable = "x",
      body = App(Var("f"), xx)
    )

  Abs("f", App(inner, inner))
}

object booleans {
  def t = Abs("t", Abs("f", body = Var("t")))
  def f = Abs("t", Abs("f", body = Var("f")))

  def and =
    lambda2(
      ("p", "q"),
      App(App(Var("p"), Var("q")), Var("p"))
    )

  def andtf = App(App(and, t), f)

  def andt = App(and, t)

  def tf  = App(t, f)
  def tft = App(tf, t)
}
