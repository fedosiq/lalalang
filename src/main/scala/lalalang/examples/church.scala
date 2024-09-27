package lalalang
package examples
package church

import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.lambda2

object booleans {
  def t = lambda2(("t", "f"), body = Var("t"))
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

object numerals {
  ???
}
