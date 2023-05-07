package lalalang.lib

import parsley.{Parsley, Result}
import parsley.character.{letterOrDigit, char, space, string, oneOf, letter}
import parsley.combinator.manyN

class LCParser:
  import parseUtils.*

  def parse(input: String): Result[String, Expr] =
    term.parse(input)

  // starts with a letter
  // may also contain digits
  val varName: Parsley[String] =
    letter
      .flatMap { head =>
        manyN(0, letterOrDigit).map(tail => (head :: tail).mkString)
      }
      .filterNot(_ == "if")

  val absName: Parsley[String] =
    oneOf('λ', '\\') *> varName <* char('.')

  val abs: Parsley[Expr.Abs] =
    for
      name <- absName
      body <- term
    yield Expr.Abs(name, body)

  // if (...) {...} else {...}
  val cond: Parsley[Expr.Cond] =
    for
      pred        <- string("if ") *> brackets(term) <* space
      trueBranch  <- squigglyBrackets(term)
      falseBranch <- string(" else ") *> squigglyBrackets(term)
    yield Expr.Cond(pred, trueBranch, falseBranch)

  val nonApp: Parsley[Expr] =
    cond <|> brackets(term) <|> abs <|> varName.map(Expr.Var(_))

  val term: Parsley[Expr] =
    chainl1(nonApp, space #> Expr.App.apply)
