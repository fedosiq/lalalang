package lalalang.lib

package util

import cats.Monad
import cats.data.StateT
import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import lalalang.lib.expr.Expr

type Id[T] = T

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

type Get[S, A]           = S => A
type PSet[B, S, T]       = B => S => T
type PUpdate[S, T, A, B] = S => (A => B) => T

trait PLens[-S, +T, +A, -B]:
  def get: Get[S, A]
  def set: PSet[B, S, T]
  def update: PUpdate[S, T, A, B] = // haven't check this yet
    s => fab => set(fab(get(s)))(s)

type Set[S, A]    = PSet[A, S, S]
type Update[S, A] = PUpdate[S, A, A, S]

type Lens[S, A] = PLens[S, S, A, A]

object Lens:
  def instance[S, A](_get: S => A, _set: A => S => S): Lens[S, A] =
    new:
      def get: S => A    = _get
      def set: Set[S, A] = _set

def lensMapState[F[_]: Monad, S, T, A](lens: Lens[S, T]): StateT[F, T, A] => StateT[F, S, A] =
  stateT =>
    StateT { s =>
      for (res, a) <- stateT.run(lens.get(s))
      yield (lens.set(res)(s), a)
    }

type ~>[-F[_], +G[_]] = [A] => F[A] => G[A]

given [F[_]: Monad, S, T](using lens: Lens[S, T]): (StateT[F, T, *] ~> StateT[F, S, *]) =
  [A] => fa => lensMapState(lens)(fa)

trait FunctorK[Alg[_[_]]]:
  def mapK[F[_], G[_]](alg: Alg[F])(f: F ~> G): Alg[G]

object FunctorK:
  def apply[Alg[_[_]]: FunctorK]: FunctorK[Alg] = summon[FunctorK[Alg]]

  object syntax:
    extension [Alg[F[_]]: FunctorK, F[_]](alg: Alg[F])
      def mapK[G[_]](using f: F ~> G): Alg[G] = FunctorK[Alg].mapK(alg)(f)
