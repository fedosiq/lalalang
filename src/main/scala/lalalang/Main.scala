package lalalang

import cats.effect.concurrent.Ref
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

  def fact(n: Int): Expr =
    Expr
      .App(Var("fact"), lit(n))
      .where(
        rec(
          "fact",
          Abs(
            "x",
            Cond(
              pred = lt(Var("x"), lit(1)),
              trueBranch = lit(1),
              falseBranch = App(Var("fact"), Var("x") - lit(1)) * Var("x")
            )
          )
        )
      )

  override def run: IO[Unit] =
    val interpreter = EnvInterpreter[IO](debug = true)
    for {
      _   <- IO(println("---" * 30))
      env <- Ref.of[IO, EnvInterpreter.Env[IO]](Map.empty)
      res <- interpreter.eval(env)(fibDirect(10))
      // res <- interpreter.eval(env)(fact(10))
      _ <- IO(println(res))

    } yield ()
