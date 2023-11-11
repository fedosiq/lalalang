package lalalang
package lib

import parsley.Parsley
import parsley.character.char
import parsley.combinator.between

object parseUtils:
  def betweenChars[T](ch1: Char, ch2: Char)(p: => Parsley[T]) = between(char(ch1), char(ch2), p)

  def brackets[T]         = betweenChars[T]('(', ')')
  def squigglyBrackets[T] = betweenChars[T]('{', '}')

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

      doParse <|> Parsley.pure(x)

    p >>= rest
