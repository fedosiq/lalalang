package lalalang

import cats.effect.{IO, IOApp}
import lalalang.examples.functions.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.*
import lalalang.examples.functions.lambda2
import lalalang.lib.interpreters.EnvInterpreter

object Main extends IOApp.Simple:
  def expr = intro(
    "y" -> lit(11),
    "x" -> (lit(3) + lit(1)),
    "f" -> lambda2(("a", "b"), mul(Var("a"), Var("b")))
  ) {
    App(App(Var("f"), Var("x")), Var("y"))
  }

  def expr2 = let("y" -> lit(11)).in {
    let("x" -> add(lit(3), lit(1)))
      .in(mul(Var("x"), Var("y")))
  }

  override def run: IO[Unit] =
    val interpreter = EnvInterpreter[IO](debug = false)
    for {
      _   <- IO(println("---" * 30))
      res <- interpreter.initEval(Map.empty)(fibDirect(10))
      // res <- interpreter.initEval(Map.empty)(fact(10))
      _ <- IO(println(res))
    } yield ()
