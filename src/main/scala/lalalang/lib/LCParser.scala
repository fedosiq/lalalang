package lalalang.lib

import lalalang.lib.expr.Expr
import lalalang.lib.expr.model.VarName
import parsley.character.{char, digit, letter, letterOrDigit, oneOf, space, string}
import parsley.combinator.many
import parsley.{Parsley, Result}

object LCParser:
  import parseUtils.*

  def parse(input: String): Result[String, Expr] =
    term.parse(input)

  private val literal: Parsley[Expr] =
    many(digit).map { numberChars =>
      Expr.Lit(numberChars.mkString.toInt)
    }

  // starts with a letter
  // may also contain digits
  private val varName: Parsley[VarName] =
    letter
      .flatMap { head =>
        many(letterOrDigit).map(tail => (head :: tail).mkString)
      }
      .filterNot(LCParser.reservedKeywords.contains)

  private val absName: Parsley[VarName] =
    oneOf('λ', '\\') *> varName <* char('.')

  private val abs: Parsley[Expr.Abs] =
    for
      name <- absName
      body <- term
    yield Expr.Abs(name, body)

  // if (...) {...} else {...}
  private val cond: Parsley[Expr.Cond] =
    for
      pred        <- string("if ") *> brackets(term) <* space
      trueBranch  <- squigglyBrackets(term)
      falseBranch <- string(" else ") *> squigglyBrackets(term)
    yield Expr.Cond(pred, trueBranch, falseBranch)

  private val nonApp: Parsley[Expr] =
    cond <|> brackets(term) <|> abs <|> varName.map(Expr.Var(_)) <|> literal

  private val term: Parsley[Expr] =
    chainl1(nonApp, space #> Expr.App.apply)

  private val reservedKeywords = Set("if", "else", "λ")
