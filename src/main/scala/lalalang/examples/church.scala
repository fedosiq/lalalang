package lalalang
package examples
package church

import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.{app, lambda2, lambdaN}

object booleans {
  val t = lambda2(("t", "f"), body = Var("t"))
  val f = Abs("t", Abs("f", body = Var("f")))

  val and =
    lambda2(
      ("p", "q"),
      App(App(Var("p"), Var("q")), Var("p"))
    )

  val andtf = App(App(and, t), f)

  val andt = App(and, t)

  val tf  = App(t, f)
  val tft = App(tf, t)
}

object numerals {
  val zero = lambda2(("f", "x"), Var("x"))
  val one  = lambda2(("f", "x"), app("f", "x"))
  val two  = lambda2(("f", "x"), app("f", app("f", "x")))

  /** Constructs normalized n-th Church numeral
    */
  def n(n: Int): Expr = {
    require(n >= 0, "Expected a non-negative number")

    val body = (0 until n).map(_ => Var("f")).foldRight(Var("x"))((f, acc) => App(f, acc))
    lambda2(("f", "x"), body)
  }

  def succ(num: Expr): Expr =
    App(
      lambdaN("n", "f", "x")(
        app("f", app(app("n", "f"), "x"))
      ),
      num
    )

  def succ_(num: Expr): Expr =
    App(
      lambdaN("n", "f", "x")(
        App(app("n", "f"), app("f", "x"))
      ),
      num
    )

  def plus(m: Expr, n: Expr): Expr =
    App(
      App(
        lambdaN("m", "n", "f", "x")(
          App(app("m", "f"), app(app("n", "f"), "x"))
        ),
        n
      ),
      m
    )

}
