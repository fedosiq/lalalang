package lalalang

import cats.MonadThrow
import cats.effect.kernel.{Ref, Sync}
import cats.effect.std.Console
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lalalang.lib.expr.Expr
import lalalang.lib.expr.Expr.given
import lalalang.lib.expr.model.VarName
import lalalang.lib.interpreters.TreeInterpreter
import lalalang.lib.parser.LCParser

class Repl[F[_]: MonadThrow: Console](
    parser: LCParser,
    interpreter: TreeInterpreter[Either[TreeInterpreter.Error, *]],
    constants: Ref[F, Map[VarName, Expr]]
):
  import Repl.{Cmd, prompt, bindRgx}

  def loop: F[Unit] =
    for
      cmd <- getCmd

      _ <- cmd match
        case Cmd.Interpret(expr) =>
          constants.get >>= (interpret(expr, _).handleErrorWith(_ => ().pure))
        case Cmd.Define(name, rawExpr) =>
          save(name, rawExpr)

      _ <- loop
    yield ()

  private def getCmd: F[Cmd] =
    for
      _     <- putStr(prompt)
      input <- Console[F].readLine
      cmd <-
        input match
          case bindRgx(name, rawExpr) =>
            Cmd.Define(name, rawExpr).pure
          case _ =>
            parseExpr(input)
              .flatTap(expr => putStrLn(s"Parsed as: ${expr.show}\nAST: ${pprint.apply(expr)}"))
              .map(Cmd.Interpret(_))
              .handleErrorWith(err => putStrLn(s"Couldn't parse: ${err}") >> getCmd)
    yield cmd

  private def parseExpr(rawExpr: String): F[Expr] =
    parser.parse(rawExpr).toEither.leftMap(new RuntimeException(_)).liftTo[F]

  private def save(name: VarName, rawExpr: String): F[Unit] =
    parseExpr(rawExpr)
      .flatMap(expr =>
        constants.getAndUpdate(_ + (name -> expr))
          >> putStrLn(s"Saved: ${name} := ${expr.show}")
      )
      .handleErrorWith(err => putStrLn(s"Couldn't save var: ${err}"))

  private def interpret(expr: Expr, constants: Map[VarName, Expr]): F[Unit] =
    for
      preprocessed <- interpreter
        .tryPreprocess(expr, constants)
        .liftTo[F]
        .handleErrorWith(err => putStrLn(s"Substitution failed: ${err}").as(expr))

      _ <- putStrLn(s"Preprocessed: ${preprocessed.show}")

      evaluated <- interpreter.eval(preprocessed).liftTo[F].onError(err => putStrLn(s"Interpret error: ${err}"))
      _         <- putStrLn(s"Evaluated: ${evaluated.show}")
      reduced   <- interpreter.reduce(evaluated).liftTo[F].onError(err => putStrLn(s"Reduce error: ${err}"))
      _         <- putStrLn(s"Reduced further: ${reduced.show}")
    yield ()

  // private def interpret_(expr: Expr, constants: Map[VarName, Expr]): Ior[Chain[String], Expr] =
  //   val preprocessed = interpreter.tryPreprocess(expr, constants) match
  //     case Left(err)  => Ior.both(Chain.one(s"Substitution failed: ${err}"), expr)
  //     case Right(res) => Ior.Right(res)

  //   preprocessed
  //     .flatMap(
  //       interpreter
  //         .eval(_)
  //         .flatMap(interpreter.reduce)
  //         .leftMap(e => Chain.one(e.getMessage))
  //         .toIor
  //     )

  private def putStrLn(str: String) = Console[F].println(str)
  private def putStr(str: String)   = Console[F].print(str)

object Repl:
  enum Cmd:
    case Interpret(expr: Expr)
    case Define(name: VarName, rawExpr: String)

  def mk[I[_]: Sync, F[_]: Sync: Console]: I[Repl[F]] =
    Ref
      .in[I, F, Map[VarName, Expr]](prelude)
      .map(Repl[F](LCParser(), TreeInterpreter[Either[TreeInterpreter.Error, *]](), _))

  val bindRgx = "(\\w+) := (.+)".r
  val prompt  = "λ> "
  val prelude = Map(
    "succ" -> examples.church.numerals.succLam,
    "add"  -> examples.church.numerals.addLam,
    "zero" -> examples.church.numerals.zero,
    "one"  -> examples.church.numerals.one,
    "two"  -> examples.church.numerals.two
  )
