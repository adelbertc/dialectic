# Dialectic
Dialectic is a (work in progress) Scala implementation of the *Kanren languages.

This whole project is a "for fun" thing for me.

### Current Status
* `dialectic-micro` contains the µKanren implementation

## `dialectic-micro`
µKanren implementation.

```scala
import dialectic.micro._
import dialectic.micro.Micro._

import scalaz.Show
import scalaz.std.string._
import scalaz.syntax.foldable._

object MicroExample0 {
  def main(args: Array[String]): Unit = {
    // (A = 7) /\ ((B = 7) \/ (B = 6))
    val a = callFresh(a => a =#= 7.const)
    val b = callFresh(b => (b =#= 5.const) \/ (b =#= 6.const))
    val query = a /\ b
    val result = query.runEmpty
    val prettyString = result.takeAll.map(Show[GoalState].shows).intercalate("\n")
    println(prettyString)

    // [(Var(0),Const(7)),(Var(1),Const(5))]
    // [(Var(0),Const(7)),(Var(1),Const(6))]
  }
}

```

### Resources
* [µKanren: A Minimal Functional Core for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)
* [jasonhemann/microKanren](https://github.com/jasonhemann/microKanren)

### License
Code provided under BSD-3 license available at http://opensource.org/licenses/BSD-3-Clause, as well as in the LICENSE
file.
