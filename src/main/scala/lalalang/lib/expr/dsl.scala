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

def rec(name: VarName, bindingBody: Expr): Binding =
  Binding(recursive = true, name, bindingBody)

def let(name: VarName, bindingBody: Expr): Binding =
  Binding(recursive = false, name, bindingBody)

def let(nameToBinding: (VarName, Expr)): Binding =
  Binding(recursive = false, nameToBinding._1, nameToBinding._2)

def intro(nameToBindings: (VarName, Expr)*)(expr: Expr): Expr =
  nameToBindings.toList match
    case last :: Nil  => let(last).in(expr)
    case head :: tail => let(head).in(intro(tail*)(expr))
    case Nil          => ???

extension (binding: Binding)
  infix def in(expr: Expr): Expr =
    Bind(binding, expr)

extension (expr: Expr)
  def where(binding: Binding): Expr =
    Bind(binding, expr)

  infix def +(expr2: Expr): Expr =
    add(expr, expr2)
  infix def -(expr2: Expr): Expr =
    sub(expr, expr2)
  infix def *(expr2: Expr): Expr =
    mul(expr, expr2)
