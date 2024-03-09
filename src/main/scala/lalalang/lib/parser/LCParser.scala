package lalalang.lib.parser

import lalalang.lib.expr.Expr.Binding
import lalalang.lib.expr.model.VarName
import lalalang.lib.expr.{ArithmeticFn, BuiltinFn, Expr}
import parsley.Parsley.*
import parsley.character.{char, digit, item, letter, letterOrDigit, oneOf, space, string}
import parsley.combinator.{option, someTill}
import parsley.debug.*
import parsley.errors.combinator.fail
import parsley.expr.chain
import parsley.{Parsley, Result, debug}

object LCParser:
  import parseUtils.*

  def parse(input: String): Result[String, Expr] =
    (term <* eof.debug("eof")).parse(input)

  private val operators =
    Set('+', '-', '*', '/')

  private val reservedKeywords =
    Set("if", "else", "λ", "let", "rec", "in") ++ operators.map(_.toString)

  // 42
  private val literal: Parsley[Expr] =
    some(digit).map { numberChars =>
      Expr.Lit(numberChars.mkString.toInt)
    }

  // x
  // starts with a letter
  // may also contain digits
  private val varName: Parsley[VarName] =
    atomic {
      letter
        .flatMap { head =>
          many(letterOrDigit).map(tail => (head :: tail).mkString)
        }
        .filterNot(reservedKeywords.contains)
    }

  // λx.
  private val absName: Parsley[VarName] =
    oneOf('λ', '\\') *> varName <* char('.')

  // λx.x
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

  // let rec x := 42 in 2*x
  private val binding: Parsley[Expr.Bind] =
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

  // 1+1*2
  // fixme: order of operations
  private val arithmetic: Parsley[Expr] = {
    // crutch to avoid infinite recursion
    val operand = literal
      | varName.map(Expr.Var(_))
      | term

    val operator =
      char('+') #> ArithmeticFn.Add
        | char('-') #> ArithmeticFn.Sub
        | char('*') #> ArithmeticFn.Mul
        | char('/') #> ArithmeticFn.Div

    val applyOp: Parsley[(Expr, Expr) => Expr] =
      operator.map(op => (a, b) => Expr.Builtin(BuiltinFn.Arithmetic(op, a, b)))

    chain.left1(operand)(applyOp)
  }

  private lazy val nonApp: Parsley[Expr] =
    binding.debug("binding")
      | cond.debug("cond")
      | parens(term).debug("in parens")
      | abs.debug("abstraction")
      | ~atomic(arithmetic.debug("arithmetic"))
      //  |  should be able to remove all the rest  |
      // \|/                                       \|/
      | varName.map(Expr.Var(_)).debug("var")
      | literal.debug("literal")

  private lazy val term: Parsley[Expr] =
    chain.left1(nonApp)(space #> Expr.App.apply)
    // chainl1(nonApp, space #> Expr.App.apply)
