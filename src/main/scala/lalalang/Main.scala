package lalalang

import cats.effect.{IO, IOApp}
import lalalang.examples.functions.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.*
import lalalang.lib.expr.dsl.*
import lalalang.examples.functions.lambda2
import lalalang.lib.interpreters.EnvInterpreter
import lalalang.lib.interpreters.bytecode.Bytecode

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
    bytecodeEval(expr2)
    // envEval(fibDirect(10))
