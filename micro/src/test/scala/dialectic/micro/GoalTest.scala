package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.{ @@, Equal, IList, Tag }
import scalaz.scalacheck.ScalazProperties.semigroup
import scalaz.Tags.{ Conjunction, Disjunction }

class GoalTest extends Specification with ScalaCheck {
  import GoalTestHelper._

  def is =
    s2"""
    Goal
      should have a lawful semigroup for conjunction ${conjSemigroupLaws}
      should have a lawful semigroup for disjunction ${disjSemigroupLaws}
    """

  def conjSemigroupLaws =
    semigroup.laws[Goal @@ Conjunction](Goal.conjunctionSemigroup, goalConjEqual, goalConjArbitrary)

  def disjSemigroupLaws =
    semigroup.laws[Goal @@ Disjunction](Goal.disjunctionSemigroup, goalDisjEqual, goalDisjArbitrary)
}

object GoalTestHelper {
  import TermTestHelper._

  implicit val goalEqual: Equal[Goal] =
    new Equal[Goal] {
      def equal(a1: Goal, a2: Goal): Boolean =
        Equal[IList[GoalState]].equal(a1.runEmpty.takeAll, a2.runEmpty.takeAll)
    }

  implicit val goalConjEqual: Equal[Goal @@ Conjunction] = goalEqual.contramap(Tag.unwrap)

  implicit val goalDisjEqual: Equal[Goal @@ Disjunction] = goalEqual.contramap(Tag.unwrap)

  implicit val goalArbitrary: Arbitrary[Goal] = {
    val unifyGen = arbitrary[Term].flatMap(l => arbitrary[Term].map(r => l =#= r))
    def conjGen: Gen[Goal] = goalGen.flatMap(l => goalGen.map(r => l /\ r))
    def disjGen: Gen[Goal] = goalGen.flatMap(l => goalGen.map(r => l \/ r))
    def freshGen: Gen[Goal] = arbFunction1[Term, Goal](Arbitrary(goalGen)).arbitrary.map(Goal.callFresh)

    def goalGen = Gen.lzy(Gen.oneOf(unifyGen, conjGen, disjGen, freshGen))

    Arbitrary(unifyGen)
  }

  implicit val goalConjArbitrary: Arbitrary[Goal @@ Conjunction] =
    Arbitrary(goalArbitrary.arbitrary.map(Tag[Goal, Conjunction]))

  implicit val goalDisjArbitrary: Arbitrary[Goal @@ Disjunction] =
    Arbitrary(goalArbitrary.arbitrary.map(Tag[Goal, Disjunction]))
}
