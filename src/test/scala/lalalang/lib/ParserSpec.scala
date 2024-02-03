package lalalang
package lib

import lalalang.functions.booleans.andtf
import lalalang.lib.Show.instances.given
import lalalang.lib.expr.Expr

class ParserSpec extends munit.FunSuite:
  import lalalang.functions.*

  val parser = LCParser

  def testParser(input: String, expected: Expr): Unit =
    parser
      .parse(input)
      .fold(
        fail(_),
        assertEquals(_, expected)
      )

  test("Variable should not start with a digit") {
    val parseResult = parser.parse("λ1f.1f")

    parseResult
      .fold(
        _ => assert(true),
        _ => fail("shouldn't have parsed")
      )
  }

  test("Should parse abstraction correctly") {
    testParser("λf.(λx.f (x x)) λx.f (x x)", lazyFixpoint)
  }

  test("Should parse application correctly with different parenthesis") {
    val expected = Expr.App(
      Expr.App(
        Expr.Abs(
          "a",
          Expr.Abs(
            "b",
            Expr.Var("a")
          )
        ),
        Expr.Lit(2)
      ),
      Expr.Lit(4)
    )

    val validExpressions = List(
      "((λa.λb.a) (2)) (4)",
      "(λa.λb.a) 2 4",
      "(λa.λb.a) (2) (4)",
      "(λa.λb.(a)) 2 4"
    )

    validExpressions.foreach(testParser(_, expected))
  }

  test("Should treat λ and \\ equally") {
    val withLambdas = "λf.(λx.f (x x)) λx.f (x x)"
    val withSlashes = withLambdas.replace('λ', '\\')

    testParser(withLambdas, lazyFixpoint)
    testParser(withSlashes, lazyFixpoint)
  }

  test("Should parse generated expression 1") {
    testParser(lazyFixpoint.show, lazyFixpoint)
  }

  test("Should parse generated expression 2") {
    testParser(andtf.show, andtf)
  }

  test("Should parse variable") {
    testParser("M", Expr.Var("M"))
  }

  test("Should parse conditional") {
    testParser(
      "if ((λx.(x)) a) {T} else {F}",
      Expr.Cond(
        Expr.App(
          Expr.Abs("x", Expr.Var("x")),
          Expr.Var("a")
        ),
        Expr.Var("T"),
        Expr.Var("F")
      )
    )
  }

  test("Should parse nested conditional") {
    testParser(
      "if (if ((λx.(x)) a) {T} else {F}) {T} else {F}",
      Expr.Cond(
        Expr.Cond(
          Expr.App(
            Expr.Abs("x", Expr.Var("x")),
            Expr.Var("a")
          ),
          Expr.Var("T"),
          Expr.Var("F")
        ),
        Expr.Var("T"),
        Expr.Var("F")
      )
    )
  }
