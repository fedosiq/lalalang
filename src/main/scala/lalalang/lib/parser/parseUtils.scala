package lalalang.lib.parser

import parsley.Parsley
import parsley.character.char
import parsley.debug.*

object parseUtils:
  def between[T](ch1: Char, ch2: Char)(p: => Parsley[T]) =
    char(ch1).debug("in") *> p.debug("inside parens term") <* char(ch2).debug("out")

  def surrounded[T](ch: Char)(p: => Parsley[T]) =
    between(ch, ch)(p)

  def parens[T]   = between[T]('(', ')')
  def brackets[T] = between[T]('{', '}')
  def spaced[T]   = surrounded[T](' ')

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
