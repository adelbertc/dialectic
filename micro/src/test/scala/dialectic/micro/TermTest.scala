package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.scalacheck.ScalazProperties.order

class TermTest extends Specification with ScalaCheck {
  import TermTestHelper._

  def is =
    s2"""
    Term
      should have a lawful ordering ${order.laws[Term]}
    """
}

object TermTestHelper {
  implicit val termArbitrary: Arbitrary[Term] = {
    val varGen = arbitrary[Int].map(Term.Var.apply)
    val constGen = arbitrary[Int].map(Term.Const.apply)
    def pairGen: Gen[Term] = termGen.flatMap(fst => termGen.map(snd => Term.Pair(fst, snd)))
    def termGen = Gen.lzy(Gen.oneOf(varGen, constGen, pairGen))

    Arbitrary(termGen)
  }
}
