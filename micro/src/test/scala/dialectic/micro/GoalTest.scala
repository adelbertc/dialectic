package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.{ @@, Equal, IList, Order, Tag }
import scalaz.scalacheck.ScalazProperties.semigroup
import scalaz.std.anyVal.intInstance
import scalaz.Tags.{ Conjunction, Disjunction }

class GoalTest extends Specification with ScalaCheck {
  import GoalTestHelper._

  def is =
    s2"""
    Goal
      should have a lawful semigroup for conjunction ${conjSemigroupLaws[Int]}
      should have a lawful semigroup for disjunction ${disjSemigroupLaws[Int]}
    """

  def conjSemigroupLaws[A : Arbitrary : Equal : Order] =
    semigroup.laws[Goal[A] @@ Conjunction](Goal.conjunctionSemigroup, goalConjEqual[A], goalConjArbitrary[A])

  def disjSemigroupLaws[A : Arbitrary : Equal : Order] =
    semigroup.laws[Goal[A] @@ Disjunction](Goal.disjunctionSemigroup, goalDisjEqual[A], goalDisjArbitrary[A])
}

object GoalTestHelper {
  import TermTestHelper._

  implicit def goalEqual[A : Equal]: Equal[Goal[A]] =
    new Equal[Goal[A]] {
      def equal(a1: Goal[A], a2: Goal[A]): Boolean =
        Equal[IList[GoalState[A]]].equal(a1.runEmpty.takeAll, a2.runEmpty.takeAll)
    }

  implicit def goalConjEqual[A : Equal]: Equal[Goal[A] @@ Conjunction] = goalEqual[A].contramap(Tag.unwrap)

  implicit def goalDisjEqual[A : Equal]: Equal[Goal[A] @@ Disjunction] = goalEqual[A].contramap(Tag.unwrap)

  implicit def goalArbitrary[A : Arbitrary : Order]: Arbitrary[Goal[A]] = {
    val unifyGen = arbitrary[Term[A]].flatMap(l => arbitrary[Term[A]].map(r => l =#= r))
    def conjGen: Gen[Goal[A]] = goalGen.flatMap(l => goalGen.map(r => l /\ r))
    def disjGen: Gen[Goal[A]] = goalGen.flatMap(l => goalGen.map(r => l \/ r))
    def freshGen: Gen[Goal[A]] = arbFunction1[Term[A], Goal[A]](Arbitrary(goalGen)).arbitrary.map(Goal.callFresh)

    def goalGen = Gen.lzy(Gen.oneOf(unifyGen, conjGen, disjGen, freshGen))

    Arbitrary(unifyGen)
  }

  implicit def goalConjArbitrary[A : Arbitrary : Order]: Arbitrary[Goal[A] @@ Conjunction] =
    Arbitrary(goalArbitrary[A].arbitrary.map(Tag[Goal[A], Conjunction]))

  implicit def goalDisjArbitrary[A : Arbitrary : Order]: Arbitrary[Goal[A] @@ Disjunction] =
    Arbitrary(goalArbitrary[A].arbitrary.map(Tag[Goal[A], Disjunction]))
}
