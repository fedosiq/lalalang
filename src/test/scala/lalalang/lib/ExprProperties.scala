package lalalang.lib

import lalalang.lib.expr.Expr
import lalalang.lib.interpreters.SubstituteTreeInterpreter
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*
import lalalang.examples.functions.*

class ExprProperties extends FunSuite with ScalaCheckSuite:

  property("Literal reduces to literal") {
    forAll { (n: Int) =>
      SubstituteTreeInterpreter.eval(lit(n)) == Expr.Lit(n)
    }
  }

  // ∀ n ∈ Z: id(n) = n
  property("Identity function evals to its argument") {
    forAll { (n: Int) =>
      SubstituteTreeInterpreter.eval(identityApply(n)) == Expr.Lit(n)
    }
  }

  property("Increment reduction increments the argument") {
    forAll { (n: Int) =>
      SubstituteTreeInterpreter.eval(incApply(n)) == Expr.Lit(n + 1)
    }
  }
