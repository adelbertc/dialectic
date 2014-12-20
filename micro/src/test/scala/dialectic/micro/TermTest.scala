package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.scalacheck.ScalazProperties.{ equal, order }
import scalaz.std.anyVal.intInstance

class TermTest extends Specification with ScalaCheck {
  import TermTestHelper._

  def is =
    s2"""
    Term
      should have a lawful equality ${equal.laws[Term[Int]]}
      should have a lawful ordering ${order.laws[Term[Int]]}
    """
}

object TermTestHelper {
  implicit def termArbitrary[A : Arbitrary]: Arbitrary[Term[A]] = {
    val varGen: Gen[Term[A]] = arbitrary[Int].map(Term.Var.apply)
    val constGen: Gen[Term[A]] = arbitrary[A].map(Term.Const.apply)
    def pairGen: Gen[Term[A]] = termGen.flatMap(fst => termGen.map(snd => Term.Pair[A](fst, snd)))
    def termGen: Gen[Term[A]] = Gen.lzy(Gen.oneOf(varGen, constGen, pairGen))

    Arbitrary(termGen)
  }
}
