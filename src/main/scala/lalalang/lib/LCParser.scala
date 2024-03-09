package lalalang.lib

import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.Binding
import lalalang.lib.expr.model.VarName
import parsley.character.{char, digit, letter, letterOrDigit, oneOf, space, string, item}
import parsley.combinator.{option, someTill}
import parsley.debug.*
import parsley.Parsley.*
import parsley.{Parsley, Result, debug}
import parsley.errors.combinator.fail

object LCParser:
  import parseUtils.*

  def parse(input: String): Result[String, Expr] =
    term.parse(input)

  private val literal: Parsley[Expr] =
    some(digit).map { numberChars =>
      Expr.Lit(numberChars.mkString.toInt)
    }

  // starts with a letter
  // may also contain digits
  private val varName: Parsley[VarName] =
    atomic {
      letter
        .flatMap { head =>
          many(letterOrDigit).map(tail => (head :: tail).mkString)
        }
        .filterNot(LCParser.reservedKeywords.contains)
    }

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
      pred        <- atomic(string("if ")) *> parens(term) <* space
      trueBranch  <- brackets(term)
      falseBranch <- string(" else ") *> brackets(term)
    yield Expr.Cond(pred, trueBranch, falseBranch)

  // let rec x := 42 in 2 * x
  private val binding: Parsley[Expr] =
    for
      _    <- string("let") <* space
      rec  <- option(string("rec ") #> true)
      name <- varName
      _    <- spaced(string(":="))

      bindChars <- someTill(item, spaced(string("in")))
      bindBody <- term.parse(bindChars.mkString) match
        case parsley.Success(a)   => pure(a)
        case parsley.Failure(err) => fail("can't parse binding body")

      inExpr <- term
    yield Expr.Bind(Binding(rec.getOrElse(false), name, bindBody), inExpr)

  private val nonApp: Parsley[Expr] =
    binding.debug("binding")
      | cond.debug("cond")
      | parens(term)
      | abs.debug("abstraction")
      | varName.map(Expr.Var(_)).debug("var")
      | literal.debug("literal")

  private val term: Parsley[Expr] =
    chainl1(nonApp, space #> Expr.App.apply)

  private val reservedKeywords = Set("if", "else", "λ", "let", "rec", "in")
