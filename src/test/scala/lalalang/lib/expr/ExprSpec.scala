package lalalang.lib.expr

import lalalang.examples.church.numerals
import lalalang.lib.interpreters.TreeInterpreter

import scala.annotation.tailrec

class ExprSpec extends munit.FunSuite:
  import numerals.*

  def eval(expr: Expr)   = TreeInterpreter.eval[Either[TreeInterpreter.Error, *]](expr).fold(throw _, identity)
  def reduce(expr: Expr) = TreeInterpreter.reduce[Either[TreeInterpreter.Error, *]](expr).fold(throw _, identity)
  val calc: Expr => Expr = reduce compose eval

  test("Numerals") {
    assertEquals(n(0), zero)
    assertEquals(n(1), one)
    assertEquals(n(2), two)
  }

  test("Succ") {
    assertEquals(calc(succ(zero)), one)
    assertEquals(calc(succ(one)), two)
  }

  test("Succ multiple applications") {
    // we cannot just write `succ(succ(one))` and expect it to be `succ(two)`,
    // we need to do `eval` in-between
    assertEquals(calc(succ(eval(succ(one)))), calc(succ(two)))
  }

  test("Succ equivalence") {
    assertEquals(calc(succ(zero)), calc(succ_(zero)))
    assertEquals(calc(succ(one)), calc(succ_(one)))
    assertEquals(calc(succ(two)), calc(succ_(two)))
  }

  test("Plus") {
    assertEquals(calc(plus(two, zero)), two)
    assertEquals(calc(plus(two, one)), calc(succ(two)))
    assertEquals(calc(plus(two, two)), n(4))
  }

  test("Smoke") {
    def churchSeq(length: Int): List[Expr] = {
      require(length >= 0, "Expected a non-negative size of sequence")

      zero :: List.unfold((zero, 1))((prevCh, cur) =>
        if cur == length + 1 then None
        else
          val nextCh = eval(succ(prevCh))
          Some((nextCh, (nextCh, cur + 1)))
      )
    }

    @tailrec
    def succN(num: Expr, times: Int): Expr = {
      require(times >= 0, "Expected a non-negative number of application times")

      times match
        case 0     => num
        case other => succN(eval(succ(num)), times - 1)
    }

    val length   = 10
    val toChurch = churchSeq(length)

    toChurch
      .map(reduce)
      .map(_.show)
      .foreach(println)

    assertEquals(reduce(toChurch(2)), two)
    assertEquals(calc(toChurch(9)), calc(succN(zero, 9)))

    (0 until length)
      .map(x => x -> toChurch(x))
      .map((x, ch) => n(x + x) -> calc(plus(ch, ch)))
      .foreach(assertEquals(_, _))
  }
