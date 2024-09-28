package lalalang
package lib

import lalalang.examples.church.booleans.andtf
import lalalang.examples.functions.*
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.*
import lalalang.lib.expr.dsl.Conversions.given
import lalalang.lib.expr.{Expr, dsl}
import lalalang.lib.parser.LCParser

class ParserSpec extends munit.FunSuite:

  val parser = LCParser()

  inline def testParser(input: String, expected: Expr): Unit =
    parser
      .parse(input)
      .fold(
        fail(_),
        assertEquals(_, expected)
      )

  inline def shouldFail(name: String)(input: String): Unit =
    test(name) {
      parser
        .parse(input)
        .fold(
          _ => assert(true),
          parsed => fail(s"shoul've failed to parse", clues(parsed))
        )
    }

  inline def shouldParse(name: String)(input: String, expected: Expr): Unit =
    test(name) {
      testParser(input, expected)
    }

  inline def shouldParse(name: String)(inputsToExpectations: (String, Expr)*): Unit =
    test(name) {
      inputsToExpectations.foreach(testParser)
    }

  shouldFail("Disallow chained compare operations") {
    "1<2<3"
  }

  shouldFail("Variable must not start with a digit") {
    "λ1f.1f"
  }

  shouldParse("Literal")("42", 42)

  shouldParse("Simplest arithmetics")(
    "2+2"     -> add(2, 2),
    "2*2+4"   -> add(mul(2, 2), 4),
    "1+1+1+1" -> add(add(add(1, 1), 1), 1)
  )

  shouldParse("Arithmetics order")(
    "1*(2+3)" -> mul(1, add(2, 3)),
    "1*2+3"   -> add(mul(1, 2), 3),
    "1+(2*3)" -> add(1, mul(2, 3))
  )

  test("Arithmetics operation priority".fail) {
    testParser("1+2*3", add(1, mul(2, 3)))
  }

  shouldParse("Comparison")(
    "1<2"       -> lt(1, 2),
    "42==42"    -> dsl.eq(42, 42),
    "2>1"       -> gt(2, 1),
    "1+2>1"     -> gt(add(1, 2), 1),
    "x>y"       -> gt(Var("x"), Var("y")),
    "(1+2)>(1)" -> gt(add(1, 2), 1)
  )

  shouldParse("Abstraction")(
    "λf.(λx.f (x x)) λx.f (x x)",
    lazyFixpoint
  )

  test("Application with different parentheses") {
    val expected = App(
      App(
        Abs(
          "a",
          Abs(
            "b",
            Var("a")
          )
        ),
        Lit(2)
      ),
      Lit(4)
    )

    val validExpressions = List(
      "((λa.λb.a) (2)) (4)",
      "(λa.λb.a) 2 4",
      "(λa.λb.a) (2) (4)",
      "(λa.λb.(a)) 2 4"
    )

    validExpressions.foreach(testParser(_, expected))
  }

  test("Treat λ and \\ equally") {
    val withLambdas = "λf.(λx.f (x x)) λx.f (x x)"
    val withSlashes = withLambdas.replace('λ', '\\')

    testParser(withLambdas, lazyFixpoint)
    testParser(withSlashes, lazyFixpoint)
  }

  shouldParse("Generated expressions")(
    lazyFixpoint.show -> lazyFixpoint,
    andtf.show        -> andtf
  )

  shouldParse("Variable")("M", Var("M"))

  shouldParse("Binding")(
    "let x := 42 in x"       -> Bind(Binding(false, "x", 42), Var("x")),
    "let rec x := 42 in x"   -> Bind(Binding(true, "x", 42), Var("x")),
    "let rec x := 42 in 1+1" -> Bind(Binding(true, "x", 42), add(1, 1)),
    "let rec x := 42 in 1+x" -> Bind(Binding(true, "x", 42), add(1, Var("x"))),
    "let rec x := 42 in x+1" -> Bind(Binding(true, "x", 42), add(Var("x"), 1)),
    "let rec x := 42 in let y := 1 in x+y" -> Bind(
      Binding(true, "x", 42),
      Bind(Binding(false, "y", 1), add(Var("x"), Var("y")))
    )
  )

  shouldParse("Conditional")(
    "if ((λx.(x)) a) {T} else {F}",
    Cond(
      App(
        Abs("x", Var("x")),
        Var("a")
      ),
      Var("T"),
      Var("F")
    )
  )

  shouldParse("Nested conditional")(
    "if (if ((λx.(x)) a) {T} else {F}) {T} else {F}",
    Cond(
      Cond(
        App(
          Abs("x", Var("x")),
          Var("a")
        ),
        Var("T"),
        Var("F")
      ),
      Var("T"),
      Var("F")
    )
  )
