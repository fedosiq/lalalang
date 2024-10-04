package lalalang.lib.parser

import parsley.Parsley
import parsley.character.char

object parseUtils:
  def between[T](ch1: Char, ch2: Char)(p: => Parsley[T]): Parsley[T] =
    char(ch1) *> p <* char(ch2)

  def surrounded[T](ch: Char)(p: => Parsley[T]): Parsley[T] =
    between(ch, ch)(p)

  def parens[T]: (=> Parsley[T]) => Parsley[T]   = between[T]('(', ')')
  def brackets[T]: (=> Parsley[T]) => Parsley[T] = between[T]('{', '}')
  def spaced[T]: (=> Parsley[T]) => Parsley[T]   = surrounded[T](' ')

  // ((M N) O) P
  // adapted from haskell's parsec
  def chainl1[A](p: Parsley[A], op: Parsley[(A, A) => A]): Parsley[A] =
    def rest(x: A): Parsley[A] =
      val doParse =
        for
          f   <- op
          y   <- p
          res <- rest(f(x, y))
        yield res

      doParse | Parsley.pure(x)

    p.flatMap(rest)
