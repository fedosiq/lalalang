package lalalang.lib.interpreters.bytecode

import cats.Monad
import cats.mtl.Stateful
import cats.syntax.all.*
import lalalang.lib.util.{FunctorK, ~>}

trait LabelManagerAlg[F[_]] {
  def add(name: String): F[LabelNum]
}

object LabelManagerAlg {
  given FunctorK[LabelManagerAlg] with
    def mapK[F[_], G[_]](alg: LabelManagerAlg[F])(f: F ~> G): LabelManagerAlg[G] =
      name => f(alg.add(name))
}

class LabelManager[F[_]: LabelManager.State: Monad] extends LabelManagerAlg[F] {
  private def appended(name: String, num: Int): F[Unit] =
    Stateful[F, LabelManager.Mappings].modify { mappings =>
      val newNum2name = mappings.num2name.appended(name)
      val newName2num = mappings.name2num + (name -> num)
      mappings.copy(newName2num, newNum2name)
    }

  def add(name: String): F[LabelNum] =
    Stateful[F, LabelManager.Mappings].get.flatMap { mappings =>
      val nextNum = mappings.num2name.length
      val uniqueName =
        if mappings.name2num.contains(name)
        then s"$name$nextNum"
        else name
      appended(uniqueName, nextNum).as(nextNum)
    }
}

object LabelManager {
  type State[F[_]] = Stateful[F, Mappings]

  case class Mappings(name2num: Map[String, LabelNum] = Map.empty, num2name: Vector[String] = Vector.empty)
}
