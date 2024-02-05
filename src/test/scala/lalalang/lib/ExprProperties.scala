package lalalang.lib

import lalalang.examples.functions.*
import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.TreeInterpreter
import lalalang.lib.interpreters.TreeInterpreter.Error
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*

class ExprProperties extends FunSuite with ScalaCheckSuite:

  def eval(e: Expr): Expr =
    TreeInterpreter.eval[Either[Error, *]](e).toOption.get

  property("Literal reduces to literal") {
    forAll { (n: Int) =>
      eval(lit(n)) == Expr.Lit(n)
    }
  }

  // ∀ n ∈ Z: id(n) = n
  property("Identity function evals to its argument") {
    forAll { (n: Int) =>
      eval(identityApply(n)) == Expr.Lit(n)
    }
  }

  property("Increment reduction increments the argument") {
    forAll { (n: Int) =>
      eval(incApply(n)) == Expr.Lit(n + 1)
    }
  }
