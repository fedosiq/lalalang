package lalalang
package lib

import lalalang.examples.functions.*
import lalalang.examples.functions.bool.andtf
import lalalang.lib.Show.instances.given
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.*
import lalalang.lib.expr.dsl
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

  inline def shouldFail(input: String): Unit =
    parser
      .parse(input)
      .fold(
        _ => assert(true),
        res => fail(s"shoul've failed to parse, but got $res")
      )

  test("Disallow chained compare operations") {
    shouldFail("1<2<3")
  }

  test("Variable must not start with a digit") {
    shouldFail("λ1f.1f")
  }

  test("Literal") {
    testParser("42", lit(42))
  }

  test("Simplest arithmetics") {
    List(
      "2+2"     -> add(lit(2), lit(2)),
      "2*2+4"   -> add(mul(lit(2), lit(2)), lit(4)),
      "1+1+1+1" -> add(add(add(lit(1), lit(1)), lit(1)), lit(1))
    ).foreach(testParser)
  }

  test("Arithmetics order") {
    List(
      "1*(2+3)" -> mul(lit(1), add(lit(2), lit(3))),
      "1*2+3"   -> add(mul(lit(1), lit(2)), lit(3)),
      "1+(2*3)" -> add(lit(1), mul(lit(2), lit(3)))
    ).foreach(testParser)
  }

  test("Arithmetics operation priority".fail) {
    testParser("1+2*3", add(lit(1), mul(lit(2), lit(3))))
  }

  test("Comparison") {
    List(
      "1<2"       -> lt(lit(1), lit(2)),
      "42==42"    -> dsl.eq(lit(42), lit(42)),
      "2>1"       -> gt(lit(2), lit(1)),
      "1+2>1"     -> gt(add(lit(1), lit(2)), lit(1)),
      "x>y"       -> gt(Var("x"), Var("y")),
      "(1+2)>(1)" -> gt(add(lit(1), lit(2)), lit(1))
    ).foreach(testParser)
  }

  test("Abstraction") {
    testParser("λf.(λx.f (x x)) λx.f (x x)", lazyFixpoint)
  }

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

  test("Parse generated expression 1") {
    testParser(lazyFixpoint.show, lazyFixpoint)
  }

  test("Parse generated expression 2") {
    testParser(andtf.show, andtf)
  }

  test("Parse variable") {
    testParser("M", Var("M"))
  }

  test("Binding") {
    List(
      "let x := 42 in x"       -> Bind(Binding(false, "x", Lit(42)), Var("x")),
      "let rec x := 42 in x"   -> Bind(Binding(true, "x", Lit(42)), Var("x")),
      "let rec x := 42 in 1+1" -> Bind(Binding(true, "x", lit(42)), add(lit(1), lit(1))),
      "let rec x := 42 in 1+x" -> Bind(Binding(true, "x", lit(42)), add(lit(1), Var("x"))),
      "let rec x := 42 in x+1" -> Bind(Binding(true, "x", lit(42)), add(Var("x"), lit(1))),
      "let rec x := 42 in let y := 1 in x+y" -> Bind(
        Binding(true, "x", lit(42)),
        Bind(Binding(false, "y", lit(1)), add(Var("x"), Var("y")))
      )
    ).foreach(testParser)
  }

  test("Conditional") {
    testParser(
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
  }

  test("Nested conditional") {
    testParser(
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
  }
