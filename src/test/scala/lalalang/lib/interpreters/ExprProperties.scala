package lalalang.lib.interpreters

import cats.effect.IO
import lalalang.examples.functions.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.dsl.*
import lalalang.lib.interpreters.TreeInterpreter.Error
import munit.{FunSuite, ScalaCheckSuite}
import org.scalacheck.Prop.*

class ExprProperties extends FunSuite with ScalaCheckSuite:

  def eval(e: Expr): Expr =
    TreeInterpreter.eval[Either[Error, *]](e).toOption.get

  val envInterpreter =
    EnvInterpreter[IO](debug = false)

  def eval2(e: Expr) =
    envInterpreter.initEval(Map.empty)(e)

  property("Literal reduces to literal") {
    forAll { (n: Int) =>
      eval(lit(n)) == lit(n)
    }
  }

  // ∀ n ∈ Z: id(n) = n
  property("Identity function evals to its argument") {
    forAll { (n: Int) =>
      eval(identityApply(n)) == lit(n)
    }
  }

  property("Increment reduction increments the argument") {
    forAll { (n: Int) =>
      eval(incApply(n)) == lit(n + 1)
    }
  }
