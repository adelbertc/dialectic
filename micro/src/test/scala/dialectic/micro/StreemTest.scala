package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.{ Equal, IList }
import scalaz.std.anyVal.intInstance
import scalaz.scalacheck.ScalazArbitrary.ilistArbitrary
import scalaz.scalacheck.ScalazProperties.monad

class StreemTest extends Specification with ScalaCheck {
  import StreemTestHelper._

  def is =
    s2"""
    Streem
      should not change from/to IList ${fromToIList[Int]}
      should be a lawful monad        ${monad.laws[Streem]}
    """

  def fromToIList[A : Arbitrary] =
    prop { (s: Streem[A]) =>
      val l = s.takeAll
      Streem.fromFoldable(l).takeAll mustEqual l
    }
}

object StreemTestHelper {
  implicit def streemArbitrary[A : Arbitrary]: Arbitrary[Streem[A]] =
    Arbitrary(arbitrary[IList[A]].map(Streem.fromFoldable[IList, A]))

  implicit def streemEqual[A : Equal]: Equal[Streem[A]] =
    Equal[IList[A]].contramap(_.takeAll)
}
