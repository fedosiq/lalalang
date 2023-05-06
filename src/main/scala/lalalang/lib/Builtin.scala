package lalalang
package lib

// TODO: abstract to 2-ary fn?
enum ArithmeticFn:
  case Add, Sub, Mul, Div

  def mapping: ArithmeticFn => (Int, Int) => Int =
    case Add => (_ + _)
    case Sub => (_ - _)
    case Mul => (_ * _)
    case Div => (_ / _)

  def apply: (Expr, Expr) => Expr.Lit =
    case (Expr.Lit(a), Expr.Lit(b)) =>
      Expr.Lit(mapping(this)(a, b))
    case _ => ???

enum ComparisonFn:
  case Lt, Eq, Gt

  def mapping: ComparisonFn => (Int, Int) => Boolean =
    case Lt => (_ < _)
    case Eq => (_ == _)
    case Gt => (_ > _)

  def apply: (Expr, Expr) => Expr.Lit =
    case (Expr.Lit(a), Expr.Lit(b)) =>
      Expr.Lit {
        if (mapping(this)(a, b)) 1
        else 0
      }
    case _ => ???

enum BuiltinFn:
  case Arithmetic(fn: ArithmeticFn, a: Expr, b: Expr)
  case Comparison(fn: ComparisonFn, a: Expr, b: Expr)

object BuiltinFn:
  def reduce(fn: BuiltinFn): Expr.Lit =
    fn match
      case Arithmetic(fn, a, b) => fn.apply(a, b)
      case Comparison(fn, a, b) => fn.apply(a, b)
