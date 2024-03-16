package lalalang.lib.parser

import lalalang.lib.expr.Expr.Binding
import lalalang.lib.expr.model.VarName
import lalalang.lib.expr.{ArithmeticFn, BuiltinFn, ComparisonFn, Expr}
import parsley.Parsley.*
import parsley.character.{char, digit, item, letter, letterOrDigit, oneOf, space, string}
import parsley.combinator.{option, someTill}
import parsley.debug.*
import parsley.errors.combinator.fail
import parsley.expr.chain
import parsley.{Parsley, Result, debug}

import scala.annotation.nowarn

class LCParser:
  import parseUtils.*
  import LCParser.*

  def parse(input: String): Result[String, Expr] =
    expression.parse(input)

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
        .filterNot(ReservedKeywords.contains)
    }

  // λx.
  private val absName: Parsley[VarName] =
    Lam *> varName <* Dot

  // λx.x
  @nowarn("msg=Could not verify that the method argument is transitively initialized")
  private val abs: Parsley[Expr.Abs] =
    for
      name <- absName
      body <- term
    yield Expr.Abs(name, body)

  // if (...) {...} else {...}
  private val cond: Parsley[Expr.Cond] =
    for
      pred        <- atomic(If) *> spaced(parens(term))
      trueBranch  <- brackets(term)
      falseBranch <- spaced(Else) *> brackets(term)
    yield Expr.Cond(pred, trueBranch, falseBranch)

  // let rec x := 42 in 2*x
  private val binding: Parsley[Expr.Bind] =
    for
      _    <- Let <* space
      rec  <- option((Rec *> space) #> true)
      name <- varName
      _    <- spaced(Bind)

      bindChars <- someTill(item, spaced(In))
      bindBody <- term.parse(bindChars.mkString) match
        case parsley.Success(a)   => pure(a)
        case parsley.Failure(err) => fail("can't parse binding body")

      inExpr <- term
    yield Expr.Bind(Binding(rec.getOrElse(false), name, bindBody), inExpr)

  // 1+1*2
  // fixme: order of operations
  private val arithmetics: Parsley[Expr] = {
    // crutch to avoid infinite recursion
    val operand =
      literal
        | varName.map(Expr.Var(_))
        | parens(term)
        | term

    val applyOp: Parsley[(Expr, Expr) => Expr] =
      arithmeticOp.map { op => (a, b) => Expr.Builtin(BuiltinFn.Arithmetic(op, a, b)) }

    chain.left1(operand)(applyOp)
  }

  // 1>2
  private val comparison =
    for
      a  <- arithmetics
      op <- comparisonOp
      b  <- arithmetics
    yield Expr.Builtin(BuiltinFn.Comparison(op, a, b))

  private lazy val nonApp: Parsley[Expr] =
    binding.debug("binding")
      | cond.debug("cond")
      | abs.debug("abstraction")
      | ~atomic(comparison.debug("comp"))
      | ~atomic(arithmetics.debug("binary op"))
      | parens(term).debug("in parens")
      //  |  should be able to remove all the rest  |
      // \|/                                       \|/
      | varName.map(Expr.Var(_)).debug("var")
      | literal.debug("literal")

  private lazy val term: Parsley[Expr] =
    chain.left1(nonApp)(space #> Expr.App.apply)

  private val expression = term <* Eof

object LCParser:
  private val lambdaChars = Set('λ', '\\')

  val Lam  = oneOf(lambdaChars)
  val Dot  = char('.')
  val If   = string("if")
  val Else = string("else")
  val Let  = string("let")
  val Rec  = string("rec")
  val Bind = string(":=")
  val In   = string("in")
  val Eof  = eof.debug("eof")

  val arithmeticOp =
    char('+') #> ArithmeticFn.Add
      | char('-') #> ArithmeticFn.Sub
      | char('*') #> ArithmeticFn.Mul
      | char('/') #> ArithmeticFn.Div

  val comparisonOp =
    char('>') #> ComparisonFn.Gt
      | char('<') #> ComparisonFn.Lt
      | string("==") #> ComparisonFn.Eq

  private val Operators =
    Set('+', '-', '*', '/', '>', '<')

  val ReservedKeywords =
    Set("if", "else", "let", "rec", "in", ":=", "==") ++ Operators.map(_.toString) ++ lambdaChars
