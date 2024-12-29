package lalalang
package examples
package church

import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.{app, lambda2, lambdaN}

object booleans {
  val t: Expr = lambda2(("t", "f"), body = Var("t"))
  val f: Expr = Abs("t", Abs("f", body = Var("f")))

  val and: Expr =
    lambda2(
      ("p", "q"),
      App(App(Var("p"), Var("q")), Var("p"))
    )

  val andtf: Expr = App(App(and, t), f)

  val andt: Expr = App(and, t)

  val tf: Expr  = App(t, f)
  val tft: Expr = App(tf, t)
}

object numerals {
  val zero: Expr = lambda2(("f", "x"), Var("x"))
  val one: Expr  = lambda2(("f", "x"), app("f", "x"))
  val two: Expr  = lambda2(("f", "x"), app("f", app("f", "x")))

  /** Constructs normalized n-th Church numeral
    */
  def n(n: Int): Expr = {
    require(n >= 0, "Expected a non-negative number")

    val body = (0 until n).map(_ => Var("f")).foldRight(Var("x"))((f, acc) => App(f, acc))
    lambda2(("f", "x"), body)
  }

  def succLam: Expr =
    lambdaN("n", "f", "x")(
      App(app("n", "f"), app("f", "x"))
    )

  def succ(num: Expr): Expr =
    App(
      lambdaN("n", "f", "x")(
        app("f", app(app("n", "f"), "x"))
      ),
      num
    )

  def succ_(num: Expr): Expr =
    App(succLam, num)

  def add(m: Expr, n: Expr): Expr =
    val lam = lambdaN("m", "n", "f", "x")(
      App(
        app("m", "f"),
        app(app("n", "f"), "x")
      )
    )

    App(App(lam, m), n)

  def add_(m: Expr, n: Expr): Expr =
    val lam = lambdaN("m", "n")(
      App(app("m", succLam), Var("n"))
    )
    App(App(lam, m), n)

  def mul(m: Expr, n: Expr): Expr =
    val lam = lambdaN("a", "b", "f")(
      app("a", app("b", "f"))
    )
    App(App(lam, m), n)

  def pow(a: Expr, b: Expr): Expr =
    val lam = lambdaN("a", "b")(app("b", "a"))
    App(App(lam, a), b)
}
