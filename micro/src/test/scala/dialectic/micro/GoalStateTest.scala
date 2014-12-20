package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.{ IMap, Order }
import scalaz.std.anyVal.intInstance
import scalaz.scalacheck.ScalazArbitrary.Arbitrary_==>>
import scalaz.scalacheck.ScalazProperties.equal

class GoalStateTest extends Specification with ScalaCheck {
  import GoalStateTestHelper._

  def is =
    s2"""
    GoalState
      should have a lawful equality ${equal.laws[GoalState[Int]]}
    """
}

object GoalStateTestHelper {
  import TermTestHelper._

  implicit def goalStateArbitrary[A : Arbitrary : Order]: Arbitrary[GoalState[A]] =
    Arbitrary(for {
      m <- arbitrary[IMap[Term[A], Term[A]]]
      i <- arbitrary[Int]
    } yield GoalState(m, i))
}
