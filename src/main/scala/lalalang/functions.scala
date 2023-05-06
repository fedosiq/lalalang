package lalalang
package functions

import lib.*

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

// lazy fixpoint
def Y = {
  val xx = App(Var("x"), Var("x"))

  val inner =
    Abs(
      variable = "x",
      body = App(Var("f"), xx)
    )

  Abs("f", App(inner, inner))
}

val fibStep = {
  def xMinus(n: Int) = Builtin(
    Arithmetic(ArithmeticFn.Sub, Var("x"), Lit(n))
  )

  val falseBranch = Builtin(
    Arithmetic(
      ArithmeticFn.Add,
      App(Var("f"), xMinus(1)),
      App(Var("f"), xMinus(2))
    )
  )

  Abs(
    "f",
    Abs(
      "x",
      Cond(
        Builtin(Comparison(ComparisonFn.Lt, Var("x"), Lit(2))),
        Lit(1),
        falseBranch
      )
    )
  )
}

def fib(n: Int) = {
  val fn = App(Y, fibStep)
  App(fn, Lit(n))
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
