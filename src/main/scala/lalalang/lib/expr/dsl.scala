package lalalang.lib.expr

package dsl

import lalalang.lib.expr.BuiltinFn.*
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.model.VarName

private type Fn2 = (Expr, Expr) => Expr.Builtin

def mkArithmetic(fn: ArithmeticFn)(a: Expr, b: Expr): Expr.Builtin =
  Builtin(Arithmetic(fn, a, b))

private def mkComparison(fn: ComparisonFn)(a: Expr, b: Expr): Expr.Builtin =
  Builtin(Comparison(fn, a, b))

val add: Fn2 = mkArithmetic(ArithmeticFn.Add)
val sub: Fn2 = mkArithmetic(ArithmeticFn.Sub)
val mul: Fn2 = mkArithmetic(ArithmeticFn.Mul)
val div: Fn2 = mkArithmetic(ArithmeticFn.Div)

val lt = mkComparison(ComparisonFn.Lt)
val gt = mkComparison(ComparisonFn.Gt)
val eq = mkComparison(ComparisonFn.Eq)

val lit: Int => Expr.Lit = Lit(_)

def app(varName: String, argName: String) = App(Var(varName), Var(argName))
def app(varName: String, arg: Expr)       = App(Var(varName), arg)
def app(body: Expr, arg: String)          = App(body, Var(arg))

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

def lambda2(variables: (String, String), body: Expr): Expr = {
  val (a, b) = variables
  Abs(a, Abs(b, body))
  // lambdaN(body, a :: b :: Nil)
}

def lambdaN(variables: String*)(body: Expr): Expr =
  variables.toList match
    case Nil          => throw Exception("cannot construct lambda for empty arg list")
    case head :: Nil  => Abs(head, body)
    case head :: tail => Abs(head, lambdaN(tail*)(body))

extension (binding: Binding)
  infix def in(expr: Expr): Expr.Bind =
    Bind(binding, expr)

extension (expr: Expr)
  def where(binding: Binding): Expr.Bind =
    Bind(binding, expr)

  infix def +(expr2: Expr): Expr =
    add(expr, expr2)
  infix def -(expr2: Expr): Expr =
    sub(expr, expr2)
  infix def *(expr2: Expr): Expr =
    mul(expr, expr2)
  infix def /(expr2: Expr): Expr =
    div(expr, expr2)

object Conversions:
  given Conversion[Int, Expr.Lit] = lit(_)
