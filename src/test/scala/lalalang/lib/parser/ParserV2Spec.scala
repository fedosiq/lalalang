package lalalang.lib.parser

import lalalang.lib.expr.dsl.add
import lalalang.lib.expr.dsl.{lit, mkComparison}
import parsley.Success
import lalalang.lib.expr.dsl.mul
import lalalang.lib.expr.ComparisonFn
import lalalang.lib.expr.Expr

class ParserV2Spec extends munit.FunSuite:
  val parse = ParserV2.parse

  test("all") {
    val examples =
      List(
        // lit
        "   1"   -> lit(1),
        "   (1)" -> lit(1),
        "(1)"    -> lit(1),
        "  x"    -> Expr.Var("x"),
        "  x  "  -> Expr.Var("x"),
        "(x)"    -> Expr.Var("x"),
        // add
        "1+1" -> add(lit(1), lit(1)),
        // spaces
        "1 +1"        -> add(lit(1), lit(1)),
        "1 + 1"       -> add(lit(1), lit(1)),
        " 1 +    1  " -> add(lit(1), lit(1)),
        " 1 +    1  " -> add(lit(1), lit(1)),
        // precedence
        "1 + 2 * 3"     -> add(lit(1), mul(lit(2), lit(3))),
        "(1 + 2) * 3"   -> mul(add(lit(1), lit(2)), lit(3)),
        "1 + 2 * 3 > 0" -> mkComparison(ComparisonFn.Gt)(add(lit(1), mul(lit(2), lit(3))), lit(0)),
        // lambda abstraction
        "\\x.x"           -> Expr.Abs("x", Expr.Var("x")),
        "λx.x"            -> Expr.Abs("x", Expr.Var("x")),
        "\\x. x"          -> Expr.Abs("x", Expr.Var("x")),
        "\\x. x + 1"      -> Expr.Abs("x", add(Expr.Var("x"), lit(1))),
        "\\x. (x + 1)"    -> Expr.Abs("x", add(Expr.Var("x"), lit(1))),
        "\\x.\\y.x+y"     -> Expr.Abs("x", Expr.Abs("y", add(Expr.Var("x"), Expr.Var("y")))),
        "\\x. \\y. x + y" -> Expr.Abs("x", Expr.Abs("y", add(Expr.Var("x"), Expr.Var("y")))),
        // application
        "f 42"         -> Expr.App(Expr.Var("f"), Expr.Lit(42)),
        "f x"          -> Expr.App(Expr.Var("f"), Expr.Var("x")),
        "f 42 43"      -> Expr.App(Expr.App(Expr.Var("f"), Expr.Lit(42)), Expr.Lit(43)),
        "\\x. f x"     -> Expr.Abs("x", Expr.App(Expr.Var("f"), Expr.Var("x"))),
        "\\x. f (x x)" -> Expr.Abs("x", Expr.App(Expr.Var("f"), Expr.App(Expr.Var("x"), Expr.Var("x")))),
        "(\\x. f) x"   -> Expr.App(Expr.Abs("x", Expr.Var("f")), Expr.Var("x")),
        // cond
        "if (1 + 1) { x } else { y }" -> Expr.Cond(add(lit(1), lit(1)), Expr.Var("x"), Expr.Var("y")),
        "if ( x ) { f x } else {y}" -> Expr.Cond(Expr.Var("x"), Expr.App(Expr.Var("f"), Expr.Var("x")), Expr.Var("y")),
        "if ( x > 0 ) { f x } else {y}" -> Expr
          .Cond(
            mkComparison(ComparisonFn.Gt)(Expr.Var("x"), Expr.Lit(0)),
            Expr.App(Expr.Var("f"), Expr.Var("x")),
            Expr.Var("y")
          ),
        "\\x. if ( x ) { f x } else {y}" -> Expr.Abs(
          "x",
          Expr.Cond(
            Expr.Var("x"),
            Expr.App(Expr.Var("f"), Expr.Var("x")),
            Expr.Var("y")
          )
        ),
        "f (if ( x ) { f x } else {y})" -> Expr.App(
          Expr.Var("f"),
          Expr.Cond(
            Expr.Var("x"),
            Expr.App(Expr.Var("f"), Expr.Var("x")),
            Expr.Var("y")
          )
        ),
        "(f) (if ( x ) { f x } else {y})" -> Expr.App(
          Expr.Var("f"),
          Expr.Cond(
            Expr.Var("x"),
            Expr.App(Expr.Var("f"), Expr.Var("x")),
            Expr.Var("y")
          )
        ),
        // binding
        "let x := 42 in x"       -> Expr.Bind(Expr.Binding(false, "x", lit(42)), Expr.Var("x")),
        "let rec x := 42 in x"   -> Expr.Bind(Expr.Binding(true, "x", lit(42)), Expr.Var("x")),
        "let rec x := 42 in 1+1" -> Expr.Bind(Expr.Binding(true, "x", lit(42)), add(lit(1), lit(1))),
        "let rec x := 42 in 1+x" -> Expr.Bind(Expr.Binding(true, "x", lit(42)), add(lit(1), Expr.Var("x"))),
        "let rec x := 42 in x+1" -> Expr.Bind(Expr.Binding(true, "x", lit(42)), add(Expr.Var("x"), lit(1))),
        "let rec x := 42 in let y := 1 in x+y" -> Expr.Bind(
          Expr.Binding(true, "x", lit(42)),
          Expr.Bind(Expr.Binding(false, "y", lit(1)), add(Expr.Var("x"), Expr.Var("y")))
        )
      )

    examples.zipWithIndex.foreach { case ((in, res), idx) =>
      assertEquals(parse(in), Success(res), s"test case #${idx}")
    }

  }
