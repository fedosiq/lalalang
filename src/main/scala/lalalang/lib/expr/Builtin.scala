package lalalang.lib.expr

import lalalang.lib.Show
import lalalang.lib.expr.dsl.lit
import lalalang.lib.util.|>

import scala.util.chaining.*

// TODO: abstract to 2-ary fn?
enum ArithmeticFn:
  case Add, Sub, Mul, Div

  private def mapping: ArithmeticFn => (Int, Int) => Int =
    case Add => (_ + _)
    case Sub => (_ - _)
    case Mul => (_ * _)
    case Div => (_ / _)

  def apply(a: Int, b: Int): Int = mapping(this)(a, b)

  def applyExpr(a: Expr, b: Expr): Expr.Lit =
    apply(Expr.asInt(a), Expr.asInt(b)) |> lit

object ArithmeticFn:
  given Show[ArithmeticFn] =
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
    case Div => "/"

enum ComparisonFn:
  case Lt, Eq, Gt

  private def mapping: ComparisonFn => (Int, Int) => Boolean =
    case Lt => (_ < _)
    case Eq => (_ == _)
    case Gt => (_ > _)

  def apply(a: Int, b: Int): Int =
    if (mapping(this)(a, b)) 1
    else 0

  def applyExpr(a: Expr, b: Expr): Expr.Lit =
    Expr.Lit(apply(Expr.asInt(a), Expr.asInt(b)))

object ComparisonFn:
  given Show[ComparisonFn] =
    case Lt => "<"
    case Eq => "=="
    case Gt => ">"

enum BuiltinFn:
  case Arithmetic(fn: ArithmeticFn, a: Expr, b: Expr)
  case Comparison(fn: ComparisonFn, a: Expr, b: Expr)

object BuiltinFn:
  given Show[BuiltinFn] =
    case Arithmetic(op, a, b) => s"${a.show} ${op.show} ${b.show}"
    case Comparison(op, a, b) => s"${a.show} ${op.show} ${b.show}"
