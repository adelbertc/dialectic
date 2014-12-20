package dialectic.micro

import scalaz.{ Contravariant, Equal, IMap, Show }
import scalaz.std.anyVal.intInstance
import scalaz.std.tuple._

final case class GoalState[A](sub: IMap[Term[A], Term[A]], nextIndex: Int)

object GoalState extends GoalStateInstances {
  /** Empty goal state with no substitutions and a starting index of 0. */
  def empty[A]: GoalState[A] = GoalState(IMap.empty, 0)
}

sealed abstract class GoalStateInstances {
  implicit def equal[A : Equal]: Equal[GoalState[A]] =
    Equal[(IMap[Term[A], Term[A]], Int)].contramap(gs => (gs.sub, gs.nextIndex))

  implicit def show[A]: Show[GoalState[A]] =
    Contravariant[Show].contramap(Show[IMap[Term[A], Term[A]]])(_.sub)
}
