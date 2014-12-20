package dialectic.micro.wrapper

import dialectic.micro.{ Goal, Term }
import dialectic.micro.Goal._

import scalaz.{ Foldable1, Tag }
import scalaz.Tags.{ Conjunction, Disjunction }

object wrapper extends WrapperHelper

/** Derived combinators */
trait WrapperHelper {
  /** Disjunction of conjunctions */
  def conde[F[_] : Foldable1, G[_] : Foldable1](fgg: F[G[Goal]]): Goal =
    Tag.unwrap(Foldable1[F].foldMap1(fgg)(fg => Tag.apply[Goal, Disjunction](conjPlus(fg))))

  /** Conjunction of goals */
  def conjPlus[F[_] : Foldable1](fg: F[Goal]): Goal =
    Tag.unwrap(Foldable1[F].foldMap1(fg)(Tag.apply[Goal, Conjunction]))

  /** Disjunction of goals */
  def disjPlus[F[_] : Foldable1](fg: F[Goal]): Goal =
    Tag.unwrap(Foldable1[F].foldMap1(fg)(Tag.apply[Goal, Disjunction]))
}
