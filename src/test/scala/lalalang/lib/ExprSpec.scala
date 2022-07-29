package lalalang
package lib

class ExprSpec extends munit.FunSuite {
  import lalalang.functions.*

  test("Literal should eval to literal") {
    assert(Expr.reduce(lit(42)) == Expr.Lit(42))
  }

  test("Identity application should eval to its argument") {
    assert(Expr.reduce(identityApply(1)) == Expr.Lit(1))
  }

  test("Should eval lambda with arithmetics") {
    assert(Expr.reduce(incApply(42)) == Expr.Lit(43))
  }

  test("T && F should be equal to TFT") {
    assert(Expr.reduce(tft) == Expr.reduce(andtf))
  }
}
