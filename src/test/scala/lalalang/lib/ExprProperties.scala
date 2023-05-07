package lalalang.lib

import munit.{ScalaCheckSuite, FunSuite}
import org.scalacheck.Prop.*

class ExprProperties extends FunSuite with ScalaCheckSuite:
  import lalalang.functions.*
  import lalalang.functions.booleans.*

  property("Literal reduces to literal") {
    forAll { (n: Int) =>
      Expr.reduce(lit(n)) == Expr.Lit(n)
    }
  }

  // ∀ n ∈ Z: id(n) = n
  property("Identity function reduces to its argument") {
    forAll { (n: Int) =>
      Expr.reduce(identityApply(n)) == Expr.Lit(n)
    }
  }

  property("Increment reduction increments the argument") {
    forAll { (n: Int) =>
      Expr.reduce(incApply(n)) == Expr.Lit(n + 1)
    }
  }
