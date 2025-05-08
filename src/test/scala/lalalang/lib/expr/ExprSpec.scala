package lalalang.lib.expr

import lalalang.examples.church.numerals
import lalalang.lib.interpreters.TreeInterpreter

import scala.annotation.tailrec

class ExprSpec extends munit.FunSuite:
  import numerals.*

  val interpreter = TreeInterpreter[Either[TreeInterpreter.Error, *]]

  def eval(expr: Expr): Expr =
    interpreter
      .eval(expr)
      .fold(throw _, identity)

  def reduce(expr: Expr): Expr =
    interpreter
      .reduce(expr)
      .fold(throw _, identity)

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
    // todo: check, is it even correct
    assertEquals(calc(succ(eval(succ(one)))), calc(succ(two)))
  }

  test("Succ equivalence") {
    List(zero, one, two)
      .map(x => calc(succ(x)) -> calc(succ_(x)))
      .foreach(assertEquals(_, _))
  }

  test("Add") {
    assertEquals(calc(add(two, zero)), two)
    assertEquals(calc(add(two, one)), calc(succ(two)))
    assertEquals(calc(add(two, two)), n(4))
  }

  test("Add equivalence") {
    assertEquals(calc(add(one, one)), calc(add_(one, one)))

    assertEquals(calc(add(one, n(3))), calc(add_(one, n(3))))

    assertEquals(calc(add(one, two)), calc(add_(one, two)))
    assertEquals(calc(add(two, one)), calc(add_(one, two)))

    // todo: need to deal with variable names
    // assertEquals(calc(add(one, two)), calc(add_(two, one)))
    // assertEquals(calc(add(two, one)), calc(add_(two, one)))
    // assertEquals(calc(add(two, two)), calc(add_(two, two)))
  }

  test("Mul") {
    val range = (1 to 4)

    (for {
      x <- range
      y <- range
    } yield x -> y)
      .map((a, b) => assertEquals(calc(mul(n(a), n(b))), n(a * b)))

  }

  test("Pow") {
    val range = (1 to 4)

    (for {
      x <- range
      y <- range
    } yield x -> y)
      .map((a, b) =>
        assertEquals(
          calc(pow(n(a), interpreter.alpha(n(b), Map("f" -> "g", "x" -> "y")))),
          interpreter.alpha(n(math.pow(a.toDouble, b.toDouble).toInt), Map("f" -> "y"))
        )
      )

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
        case 0 => num
        case _ => succN(eval(succ(num)), times - 1)
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
      .map((x, ch) => n(x + x) -> calc(add(ch, ch)))
      .foreach(assertEquals(_, _))
  }
