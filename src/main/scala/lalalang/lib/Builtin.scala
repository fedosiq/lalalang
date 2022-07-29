package lalalang
package lib

// TODO: abstract to 2-ary fn?
enum ArithmeticFn:
  case Add, Sub, Mul, Div

  def mapping: Map[ArithmeticFn, (Int, Int) => Int] =
    Map(
      Add -> (_ + _),
      Sub -> (_ - _),
      Mul -> (_ * _),
      Div -> (_ / _)
    )

  def apply(a: Expr, b: Expr): Expr.Lit =
    (a, b) match
      case (Expr.Lit(v1), Expr.Lit(v2)) =>
        Expr.Lit(mapping(this)(v1, v2))
      case _ => ???

enum ComparisonFn:
  case Lt, Eq, Gt

  def mapping: Map[ComparisonFn, (Int, Int) => Boolean] =
    Map(
      Lt -> (_ < _),
      Eq -> (_ == _),
      Gt -> (_ > _)
    )

  def apply(a: Expr, b: Expr): Expr.Lit =
    (a, b) match
      case (Expr.Lit(v1), Expr.Lit(v2)) =>
        Expr.Lit {
          if (mapping(this)(v1, v2)) 1
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
