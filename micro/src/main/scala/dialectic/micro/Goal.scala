package dialectic.micro

import dialectic.micro.Term._

import scalaz.{ @@, Equal, IMap, Maybe, Order, Ordering, Semigroup, Show, Tag }
import scalaz.std.anyVal.intInstance
import scalaz.Tags.{ Conjunction, Disjunction }

/** A goal is a function `GoalState => Streem[GoalState]`.
  *
  * A goal can be thought of as a function that takes in an initial [[dialectic.micro.GoalState]]
  * and produces a [[dialectic.micro.Streem]] of satisfying [[dialectic.micro.GoalState]]s.
  *
  * There are four primitive combinators to create a [[dialectic.micro.Goal]]:
  *  - `a \/ b` takes the disjunction of goals `a` and `b`
  *  - `a /\ b` takes the conjunction of goals `a` and `b`
  *  - `callFresh` takes a function `Term => Goal`, and creates a fresh [[dialectic.micro.Term]] to use in the body
  *  - `s =:= t` requires that `s` unify with `t`
  */
sealed abstract class Goal (private val goal: GoalState => Streem[GoalState]) {
  /** Disjunction */
  def \/(other: => Goal): Goal = new Goal(gs => run(gs).merge(Streem.delay(other.run(gs)))) {}

  /** Conjunction */
  def /\(other: Goal): Goal = new Goal(gs => { run(gs).mergeMap(other.run) }) {}

  /** Run this goal against an initial [[dialectic.micro.GoalState]]. */
  def run(goalState: GoalState): Streem[GoalState] = goal(goalState)

  /** Run this goal against the empty state */
  def runEmpty: Streem[GoalState] = run(GoalState.empty)
}

object Goal extends GoalHelper

trait GoalHelper {
  /** Creates a fresh [[dialectic.micro.Term.Var]] and passes it into the function. */
  def callFresh(f: Term => Goal): Goal =
    new Goal(gs => {
      val v = Var(gs.nextIndex)
      f(v).run(gs.copy(nextIndex = gs.nextIndex + 1))
    }) {}

  /** Semigroup instance for combining [[dialectic.micro.Goal]]s via conjunction `Goal#/\`. */
  implicit val conjunctionSemigroup: Semigroup[Goal @@ Conjunction] =
    new Semigroup[Goal @@ Conjunction] {
      def append(f1: Goal @@ Conjunction, f2: => Goal @@ Conjunction): Goal @@ Conjunction =
        Tag.apply(Tag.unwrap(f1) /\ Tag.unwrap(f2))
    }

  /** Semigroup instance for combining [[dialectic.micro.Goal]]s via disjunction `Goal#\/ */
  implicit val disjunctionSemigroup: Semigroup[Goal @@ Disjunction] =
    new Semigroup[Goal @@ Disjunction] {
      def append(f1: Goal @@ Disjunction, f2: => Goal @@ Disjunction): Goal @@ Disjunction =
        Tag.apply(Tag.unwrap(f1) \/ Tag.unwrap(f2))
    }
}

sealed abstract class Term {
  /** Create a [[dialectic.micro.Goal]] requiring this `Term` unify with `other`. */
  def =#=(other: Term): Goal =
    new Goal(gs => {
      Term.unify(this, other, gs.sub).cata(newSub => Streem(gs.copy(sub = newSub)),
                                           Streem.empty)
    }) {}
}

object Term extends TermInstances with TermFunctions {
  final case class Var(index: Int) extends Term
  final case class Const(value: Int) extends Term
  final case class Pair(first: Term, second: Term) extends Term

  private def unify(u: Term, v: Term, sub: IMap[Term, Term]): Maybe[IMap[Term, Term]] = {
    val uw = walk(u, sub)
    val vw = walk(v, sub)
    (uw, vw) match {
      case (Var(uv), Var(vv)) if Equal[Int].equal(uv, vv) => Maybe.just(sub)
      case (Var(_), _) => extend(uw, vw, sub)
      case (_, Var(_)) => extend(vw, uw, sub)
      case (Pair(x1, y1), Pair(x2, y2)) => unify(x1, x2, sub).flatMap(newSub => unify(y1, y2, newSub))
      case (uu, vu) => if (Equal[Term].equal(uu, vu)) Maybe.just(sub) else Maybe.empty
    }
  }
}

sealed abstract class TermInstances {
  implicit val order: Order[Term] =
    new Order[Term] {
      def order(x: Term, y: Term): Ordering =
        x match {
          case Var(i1) =>
            y match {
              case Var(i2) => Order[Int].order(i1, i2)
              case Const(_) => Ordering.LT
              case Pair(_, _) => Ordering.LT
            }
          case Const(v1) =>
            y match {
              case Var(_) => Ordering.GT
              case Const(v2) => Order[Int].order(v1, v2)
              case Pair(_, _) => Ordering.LT
            }
          case Pair(f1, s1) =>
            y match {
              case Var(_) => Ordering.GT
              case Const(_) => Ordering.GT
              case Pair(f2, s2) => Semigroup[Ordering].append(order(f1, f2), order(s1, s2))
            }
        }
    }

  implicit val show: Show[Term] = Show.showFromToString
}

sealed trait TermFunctions {
  protected def walk(t: Term, sub: IMap[Term, Term]): Term =
    t match {
      case v@Term.Var(_) => sub.lookup(v).fold(t)(vt => walk(vt, sub))
      case _ => t
    }

  protected def extend(x: Term, v: Term, sub: IMap[Term, Term]): Maybe[IMap[Term, Term]] =
    x match {
      case vr@Term.Var(_) => Maybe.just(sub.insert(vr, v))
      case _ => Maybe.empty
    }
}
