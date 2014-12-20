package dialectic.micro

import scalaz.{ Contravariant, Equal, IMap, Show }
import scalaz.std.anyVal.intInstance
import scalaz.std.tuple._

final case class GoalState(sub: IMap[Term, Term], nextIndex: Int)

object GoalState extends GoalStateInstances {
  /** Empty goal state with no substitutions and a starting index of 0. */
  val empty: GoalState = GoalState(IMap.empty, 0)
}

sealed abstract class GoalStateInstances {
  implicit val equal: Equal[GoalState] =
    Equal[(IMap[Term, Term], Int)].contramap(gs => (gs.sub, gs.nextIndex))

  implicit val show: Show[GoalState] =
    Contravariant[Show].contramap(Show[IMap[Term, Term]])(_.sub)
}
