package lalalang.lib.expr

package dsl

import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.model.VarName

private type Fn2 = (Expr, Expr) => Expr

private def mkArithmetic(fn: ArithmeticFn)(a: Expr, b: Expr): Expr =
  Builtin(Arithmetic(fn, a, b))

private def mkComparison(fn: ComparisonFn)(a: Expr, b: Expr): Expr =
  Builtin(Comparison(fn, a, b))

val add: Fn2 = mkArithmetic(ArithmeticFn.Add)
val sub: Fn2 = mkArithmetic(ArithmeticFn.Sub)
val mul: Fn2 = mkArithmetic(ArithmeticFn.Mul)
val div: Fn2 = mkArithmetic(ArithmeticFn.Div)

val lt = mkComparison(ComparisonFn.Lt)
val gt = mkComparison(ComparisonFn.Gt)
val eq = mkComparison(ComparisonFn.Eq)

def rec(name: VarName, bindingBody: Expr) =
  Binding(recursive = true, name, bindingBody)

extension (binding: Binding)
  def in(expr: Expr): Expr =
    Bind(binding, expr)

extension (expr: Expr)
  def where(binding: Binding): Expr =
    Bind(binding, expr)
