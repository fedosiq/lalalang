package lalalang
package lib

import Show.instances.given

class ParserSpec extends munit.FunSuite:
  import lalalang.functions.*

  val parser = LCParser()

  def testParser(input: String, expected: Expr): Unit =
    parser
      .parse(input)
      .fold(
        fail(_),
        assertEquals(_, expected)
      )

  test("Should parse exprsession correctly") {
    testParser("λf.(λx.f (x x)) λx.f (x x)", Y)
  }

  test("Should parse generated expression") {
    testParser(Y.show, Y)
  }

  test("Should parse variable") {
    testParser("M", Expr.Var("M"))
  }
