package lalalang
package examples
package functions

import lalalang.lib.expr.ArithmeticFn.*
import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.*
import lalalang.lib.expr.dsl.*

def twoTimesTwo    = mul(lit(2), lit(2))
def twoPlus3Times4 = add(lit(2), mul(lit(3), lit(4)))

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

// will recurse forever in case of eager arg evaluation
def lazyFixpoint = {
  val xx = App(Var("x"), Var("x"))

  val inner =
    Abs(
      variable = "x",
      body = App(Var("f"), xx)
    )

  Abs("f", App(inner, inner))
}

def eagerFixpoint = {
  val xx          = App(Var("x"), Var("x"))
  val indirection = Abs("v", App(xx, Var("v")))

  val inner =
    Abs(
      variable = "x",
      body = App(Var("f"), indirection)
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

val fibStepDsl = {
  def xMinus(n: Int) = Var("x") - Lit(n)

  val falseBranch =
    App(Var("f"), xMinus(1)) + App(Var("f"), xMinus(2))

  lambda2(
    ("f", "x"),
    Cond(
      dsl.lt(Var("x"), Lit(2)),
      Lit(1),
      falseBranch
    )
  )

}

def fib(n: Int, _lazy: Boolean) = {
  val fn = App(if (_lazy) lazyFixpoint else eagerFixpoint, fibStep)
  App(fn, Lit(n))
}

def fibDirect(n: Int) = {
  def xMinus(n: Int) = Builtin(
    Arithmetic(ArithmeticFn.Sub, Var("x"), Lit(n))
  )

  val falseBranch = Builtin(
    Arithmetic(
      ArithmeticFn.Add,
      App(Var("fib"), xMinus(1)),
      App(Var("fib"), xMinus(2))
    )
  )
  // let rec fib = \x -> if (x < 2) 1 else fib (x - 1) + fib (x - 2)
  Bind(
    Binding(
      recursive = true,
      name = "fib",
      bindingBody = Abs(
        variable = "x",
        body = Cond(
          pred = Builtin(Comparison(ComparisonFn.Lt, Var("x"), Lit(2))),
          trueBranch = Lit(1),
          falseBranch = falseBranch
        )
      )
    ),
    expr = App(Var("fib"), lit(n))
  )
}

def fact(n: Int): Expr =
  App(Var("fact"), lit(n))
    .where(
      rec(
        "fact",
        Abs(
          "x",
          Cond(
            pred = lt(Var("x"), lit(1)),
            trueBranch = lit(1),
            falseBranch = App(Var("fact"), Var("x") - lit(1)) * Var("x")
          )
        )
      )
    )

def diverging = rec("x", Var("x") + lit(1)).in(Var("x"))
