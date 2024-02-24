package lalalang.lib

package util

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all.*
import lalalang.lib.expr.Expr

extension [A](a: A)
  inline infix def |>[B](f: A => B): B =
    f(a)

def tree(expr: Expr, indent: Int): String =
  val spaces = "\n" + " " * indent
  expr match
    case v: Expr.Var => spaces + v.toString
    case Expr.Abs(variable, body) =>
      spaces + s"Abs(\n${" " * (indent + 2)}${variable},${tree(body, indent + 2)}$spaces)"

    case Expr.App(expr, arg) =>
      spaces + s"App(${tree(expr, indent + 2)},${tree(arg, indent + 2)}$spaces)"

    case l: Expr.Lit      => spaces + l.toString
    case Expr.Builtin(fn) => spaces + fn.toString

    case Expr.Cond(pred, trueBranch, falseBranch) =>
      spaces + s"Cond(${tree(pred, indent + 2)},${tree(trueBranch, indent + 2)},${tree(falseBranch, indent + 2)}"
    case _: Expr.Bind => ???

def timed[A](a: => A): (A, Long) = {
  val start = System.currentTimeMillis
  val res   = a
  val time  = System.currentTimeMillis - start
  res -> time
}

def cloneMap[F[_]: Sync, A, B](ref: Ref[F, A], f: A => B): F[Ref[F, B]] =
  ref.get.map(f).flatMap(Ref.of)
