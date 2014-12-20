package dialectic.example.micro

import dialectic.micro._
import dialectic.micro.Micro._

import scalaz.Show
import scalaz.std.string._
import scalaz.syntax.foldable._

object MicroExample0 {
  def stringify[A : Show](r: Streem[A], n: Int): String =
    r.take(n).takeAll.map(Show[A].shows).intercalate("\n")

  def main(args: Array[String]): Unit = {
    // (A = 7) /\ ((B = 7) \/ (B = 6))
    val a = callFresh(a => a =#= 7.const)
    val b = callFresh(b => (b =#= 5.const) \/ (b =#= 6.const))
    val query = a /\ b
    val result = query.runEmpty
    println(stringify(result, 10) ++ "\n")

    // (C = 5) \/ (C = 5) \/ ...
    def fives(c: Term): Goal = (c =#= 5.const) \/ fives(c)
    val result2 = callFresh(fives).runEmpty
    println(stringify(result2, 10) ++ "\n")

    // (X = 5) \/ (X = 6) \/ (X = 5) \/ (X = 6) \/ ...
    def sixes(c: Term): Goal = (c =#= 6.const) \/ sixes(c)
    val fivesAndSixes = callFresh(x => fives(x) \/ sixes(x))
    val result3 = fivesAndSixes.runEmpty
    println(stringify(result3, 10) ++ "\n")
  }
}
