package lalalang
package lib

import parsley.Parsley
import parsley.character.char
import parsley.combinator.between

object parseUtils:
  def parens[T](p: => Parsley[T]): Parsley[T] = between(char('('), char(')'), p)

  // ((M N) O) P
  // adapted from haskell's parsec
  def chainl1[A](p: Parsley[A], op: Parsley[(A, A) => A]): Parsley[A] =
    // TODO: replace with foldLeftM?
    def rest(x: A): Parsley[A] =
      val doParse =
        for
          f   <- op
          y   <- p
          res <- rest(f(x, y))
        yield res

      doParse <|> Parsley.pure(x)

    p >>= rest
