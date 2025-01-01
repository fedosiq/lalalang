package lalalang

import cats.effect.{IO, IOApp}

object ReplApp extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      _    <- IO.println("Starting...")
      repl <- Repl.mk[IO, IO]
      _    <- repl.loop.guarantee(IO.println("\nBye!"))
    yield ()
