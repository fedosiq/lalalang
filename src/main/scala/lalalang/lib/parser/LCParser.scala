package lalalang.lib.parser

import lalalang.lib.expr.Expr.Binding
import lalalang.lib.expr.dsl.mkArithmetic
import lalalang.lib.expr.model.VarName
import lalalang.lib.expr.{ArithmeticFn, BuiltinFn, ComparisonFn, Expr}
import parsley.Parsley.*
import parsley.character.{char, digit, item, letter, letterOrDigit, oneOf, space, string}
import parsley.combinator.{option, someTill}
import parsley.errors.combinator.fail
import parsley.expr.chain
import parsley.{Parsley, Result}

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
      arithmeticOp.map(mkArithmetic)

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
    binding
      | cond
      | abs
      | ~atomic(comparison)
      | ~atomic(arithmetics)
      | parens(term)
      //  |  should be able to remove all the rest  |
      // \|/                                       \|/
      | varName.map(Expr.Var(_))
      | literal

  private lazy val term: Parsley[Expr] =
    chain.left1(nonApp)(space #> Expr.App.apply)

  private val expression = term <* Eof

object LCParser:
  private val lambdaChars = Set('λ', '\\')

  val Lam: Parsley[Char]    = oneOf(lambdaChars)
  val Dot: Parsley[Char]    = char('.')
  val If: Parsley[String]   = string("if")
  val Else: Parsley[String] = string("else")
  val Let: Parsley[String]  = string("let")
  val Rec: Parsley[String]  = string("rec")
  val Bind: Parsley[String] = string(":=")
  val In: Parsley[String]   = string("in")
  val Eof                   = eof

  val arithmeticOp: Parsley[ArithmeticFn] =
    char('+') #> ArithmeticFn.Add
      | char('-') #> ArithmeticFn.Sub
      | char('*') #> ArithmeticFn.Mul
      | char('/') #> ArithmeticFn.Div

  val comparisonOp: Parsley[ComparisonFn] =
    char('>') #> ComparisonFn.Gt
      | char('<') #> ComparisonFn.Lt
      | string("==") #> ComparisonFn.Eq

  private val Operators: Set[Char] =
    Set('+', '-', '*', '/', '>', '<')

  val ReservedKeywords: Set[String] =
    Set("if", "else", "let", "rec", "in", ":=", "==")
      ++ Operators.map(_.toString)
      ++ lambdaChars.map(_.toString)
