package lalalang.lib.parser

import lalalang.lib.expr.dsl.{mkArithmetic, mkComparison}
import lalalang.lib.expr.model.VarName
import lalalang.lib.expr.{ArithmeticFn, ComparisonFn, Expr}
import parsley.Parsley.*
import parsley.character.{char, digit, item, letter, letterOrDigit, oneOf, space, string}
import parsley.expr.{InfixL, Ops, precedence}
import parsley.{Parsley, Result}
import parsley.combinator.{option, someTill}
import parsley.errors.combinator.{fail}

object ParserV2:
  import parseUtils.*
  import Lex.*

  def parse(input: String): Result[String, Expr] =
    expression.parse(input)

  private lazy val int: Parsley[Expr] =
    some(digit).map { numberChars =>
      Expr.Lit(numberChars.mkString.toInt)
    }

  private val varName: Parsley[VarName] =
    letter
      .flatMap { head =>
        many(letterOrDigit).map(tail => (head :: tail).mkString)
      }
      .filterNot(ReservedKeywords.contains)

  private lazy val atom = int
    | varName.map(Expr.Var(_))
    | parens(expr)

  private lazy val operations: Parsley[Expr] = {
    val operand = spaced(atom)

    precedence(operand)(
      Ops(InfixL)(
        char('*').as(mkArithmetic(ArithmeticFn.Mul)),
        char('/').as(mkArithmetic(ArithmeticFn.Div))
      ),
      Ops(InfixL)(
        char('+').as(mkArithmetic(ArithmeticFn.Add)),
        char('-').as(mkArithmetic(ArithmeticFn.Sub))
      ),
      Ops(InfixL)(
        char('>').as(mkComparison(ComparisonFn.Gt)),
        char('<').as(mkComparison(ComparisonFn.Lt))
      ),
      Ops(InfixL)(string("==").as(mkComparison(ComparisonFn.Eq))),
      Ops(InfixL)((many(space) #> Expr.App.apply))
    )
  }

  private val abs: Parsley[Expr.Abs] =
    val absName = Lam *> varName <* Dot
    for
      name <- absName
      body <- spaced(expr)
    yield Expr.Abs(name, body)

  private val cond: Parsley[Expr.Cond] =
    for
      pred        <- atomic(If) *> spaced(parens(expr))
      trueBranch  <- brackets(expr)
      falseBranch <- spaced(Else) *> brackets(expr)
    yield Expr.Cond(pred, trueBranch, falseBranch)

  private val binding: Parsley[Expr.Bind] =
    for
      _    <- Let <* space
      rec  <- option((Rec *> space) #> true)
      name <- varName
      _    <- spaced(Bind)

      bindChars <- someTill(item, spaced(In))
      // todo: should be a better way to do this
      bindBody <- expr.parse(bindChars.mkString) match
        case parsley.Success(a)   => pure(a)
        case parsley.Failure(err) => fail(s"can't parse binding body: $err")

      inExpr <- expr
    yield Expr.Bind(Expr.Binding(rec.getOrElse(false), name, bindBody), inExpr)

  private val expr = binding | abs | cond | operations

  private lazy val expression = expr <* eof

object Lex:
  private val lambdaChars = Set('λ', '\\')

  val Lam: Parsley[Char]    = oneOf(lambdaChars)
  val Dot: Parsley[Char]    = char('.')
  val If: Parsley[String]   = string("if")
  val Else: Parsley[String] = string("else")
  val Let: Parsley[String]  = string("let")
  val Rec: Parsley[String]  = string("rec")
  val Bind: Parsley[String] = string(":=")
  val In: Parsley[String]   = string("in")

  private val Operators: Set[Char] =
    Set('+', '-', '*', '/', '>', '<')

  val ReservedKeywords: Set[String] =
    Set("if", "else", "let", "rec", "in", ":=", "==")
      ++ Operators.map(_.toString)
      ++ lambdaChars.map(_.toString)
