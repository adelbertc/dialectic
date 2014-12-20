package dialectic.micro

import org.scalacheck._
import org.scalacheck.Arbitrary._

import org.specs2._

import scalaz.IList
import scalaz.scalacheck.ScalazArbitrary.ilistArbitrary

class StreemTest extends Specification with ScalaCheck with StreemTestHelper {
  def is =
    s2"""
    Streem
      should not change from/to IList ${fromToIList[Int]}
    """

  def fromToIList[A : Arbitrary] =
    prop { (s: Streem[A]) =>
      val l = s.takeAll
      Streem.fromFoldable(l).takeAll mustEqual l
    }
}

sealed trait StreemTestHelper {
  implicit def streemArbitrary[A : Arbitrary]: Arbitrary[Streem[A]] =
    Arbitrary(arbitrary[IList[A]].map { l => Streem.fromFoldable(l.take(100)) })
}
