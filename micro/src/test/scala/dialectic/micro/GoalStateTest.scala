package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.IMap
import scalaz.scalacheck.ScalazArbitrary.Arbitrary_==>>
import scalaz.scalacheck.ScalazProperties.equal

class GoalStateTest extends Specification with ScalaCheck {
  import GoalStateTestHelper._

  def is =
    s2"""
    GoalState
      should have a lawful equality ${equal.laws[GoalState]}
    """
}

object GoalStateTestHelper {
  import TermTestHelper._

  implicit val goalStateArbitrary: Arbitrary[GoalState] =
    Arbitrary(for {
      m <- arbitrary[IMap[Term, Term]]
      i <- arbitrary[Int]
    } yield GoalState(m, i))
}
