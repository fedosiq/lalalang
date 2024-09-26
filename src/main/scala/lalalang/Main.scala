package lalalang

import cats.effect.{IO, IOApp}
import cats.syntax.all.*
import lalalang.examples.functions.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.*
import lalalang.lib.interpreters.EnvInterpreter
import lalalang.lib.interpreters.bytecode.Bytecode

object Main extends IOApp.Simple:
  val expr = intro(
    "y" -> lit(11),
    "x" -> (lit(3) + lit(1)),
    "f" -> lambda2(("a", "b"), Var("a") * Var("b"))
  ) {
    App(App(Var("f"), Var("x")), Var("y"))
  }

  val expr2 = let("y" -> lit(11)).in {
    let("x" -> (lit(3) + lit(1)))
      .in(Var("x") * Var("y"))
  }

  val expr3 = lambda2(("a", "b"), Var("a") * Var("b"))

  val expr4 = App(App(lambda2(("a", "b"), Var("a") * Var("b")), lit(42)), lit(2))

  val exprs = List(expr, expr2, expr3, expr4)

  private def log[T](str: T): IO[Unit] =
    IO(println(str))

  def envEval(expr: Expr) =
    val interpreter = EnvInterpreter[IO](debug = false)

    for {
      _   <- log("---" * 30)
      res <- interpreter.initEval(Map.empty)(expr)
      _   <- log(res)
    } yield ()

  def bytecodeEval(expr: Expr) =
    IO.pure(Bytecode.eval(expr))
      .flatMap(log)

  override def run: IO[Unit] =
    exprs.traverse_(bytecodeEval)
    // bytecodeEval(expr4)
    // envEval(fibDirect(10))
