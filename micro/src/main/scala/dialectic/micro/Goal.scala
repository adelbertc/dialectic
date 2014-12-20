package dialectic.micro

import dialectic.micro.Term._

import scalaz.{ @@, Equal, IMap, Maybe, Order, Ordering, Semigroup, Show, Tag }
import scalaz.std.anyVal.intInstance
import scalaz.Tags.{ Conjunction, Disjunction }

/** A goal is a function `GoalState[A] => Streem[GoalState[A]]`.
  *
  * The type parameter indicates the type of the constants being unified against.
  *
  * A goal can be thought of as a function that takes in an initial [[dialectic.micro.GoalState]]
  * and produces a [[dialectic.micro.Streem]] of satisfying [[dialectic.micro.GoalState]]s.
  *
  * There are four primitive combinators to create a [[dialectic.micro.Goal]]:
  *  - `a \/ b` takes the disjunction of goals `a` and `b`
  *  - `a /\ b` takes the conjunction of goals `a` and `b`
  *  - `callFresh` takes a function `Term[A] => Goal[A]`, and creates a fresh [[dialectic.micro.Term]] to use in the body
  *  - `s =:= t` requires that `s` unify with `t`
  */
sealed abstract class Goal[A](private val goal: GoalState[A] => Streem[GoalState[A]]) {
  /** Disjunction */
  def \/(other: => Goal[A]): Goal[A] = new Goal[A](gs => run(gs).merge(Streem.delay(other.run(gs)))) {}

  /** Conjunction */
  def /\(other: Goal[A]): Goal[A] = new Goal[A](gs => { run(gs).mergeMap(other.run) }) {}

  /** Run this goal against an initial [[dialectic.micro.GoalState]]. */
  def run(goalState: GoalState[A]): Streem[GoalState[A]] = goal(goalState)

  /** Run this goal against the empty state */
  def runEmpty: Streem[GoalState[A]] = run(GoalState.empty[A])
}

object Goal extends GoalHelper

trait GoalHelper {
  /** Creates a fresh [[dialectic.micro.Term.Var]] and passes it into the function. */
  def callFresh[A](f: Term[A] => Goal[A]): Goal[A] =
    new Goal[A](gs => {
      val v = Var[A](gs.nextIndex)
      f(v).run(gs.copy(nextIndex = gs.nextIndex + 1))
    }) {}

  /** Semigroup instance for combining [[dialectic.micro.Goal]]s via conjunction `Goal#/\`. */
  implicit def conjunctionSemigroup[A]: Semigroup[Goal[A] @@ Conjunction] =
    new Semigroup[Goal[A] @@ Conjunction] {
      def append(f1: Goal[A] @@ Conjunction, f2: => Goal[A] @@ Conjunction): Goal[A] @@ Conjunction =
        Tag.apply(Tag.unwrap(f1) /\ Tag.unwrap(f2))
    }

  /** Semigroup instance for combining [[dialectic.micro.Goal]]s via disjunction `Goal#\/` */
  implicit def disjunctionSemigroup[A]: Semigroup[Goal[A] @@ Disjunction] =
    new Semigroup[Goal[A] @@ Disjunction] {
      def append(f1: Goal[A] @@ Disjunction, f2: => Goal[A] @@ Disjunction): Goal[A] @@ Disjunction =
        Tag.apply(Tag.unwrap(f1) \/ Tag.unwrap(f2))
    }
}

sealed abstract class Term[A] {
  /** Create a [[dialectic.micro.Goal]] requiring this `Term` unify with `other`. */
  def =#=(other: Term[A])(implicit A: Order[A]): Goal[A] =
    new Goal[A](gs => {
      Term.unify[A](this, other, gs.sub).cata(newSub => Streem(gs.copy(sub = newSub)),
                                           Streem.empty)
    }) {}
}

object Term extends TermInstances with TermFunctions {
  final case class Var[A](index: Int) extends Term[A]
  final case class Const[A](value: A) extends Term[A]
  final case class Pair[A](first: Term[A], second: Term[A]) extends Term[A]

  private def unify[A : Order](u: Term[A], v: Term[A], sub: IMap[Term[A], Term[A]]): Maybe[IMap[Term[A], Term[A]]] = {
    val uw = walk(u, sub)
    val vw = walk(v, sub)
    (uw, vw) match {
      case (Var(uv), Var(vv)) if Equal[Int].equal(uv, vv) => Maybe.just(sub)
      case (Var(_), _) => extend(uw, vw, sub)
      case (_, Var(_)) => extend(vw, uw, sub)
      case (Pair(x1, y1), Pair(x2, y2)) => unify(x1, x2, sub).flatMap(newSub => unify(y1, y2, newSub))
      case (uu, vu) => if (Equal[Term[A]].equal(uu, vu)) Maybe.just(sub) else Maybe.empty
    }
  }
}

sealed abstract class TermInstances {
  implicit def equal[A : Equal] : Equal[Term[A]] =
    new Equal[Term[A]] {
      def equal(a1: Term[A], a2: Term[A]): Boolean =
        (a1, a2) match {
          case (Var(i1), Var(i2)) => Equal[Int].equal(i1, i2)
          case (Const(v1), Const(v2)) => Equal[A].equal(v1, v2)
          case (Pair(l1, r1), Pair(l2, r2)) => equal(l1, l2) && equal(r1, r2)
          case (_, _) => false
        }
    }

  implicit def order[A : Order]: Order[Term[A]] =
    new Order[Term[A]] {
      def order(x: Term[A], y: Term[A]): Ordering =
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
              case Const(v2) => Order[A].order(v1, v2)
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

  implicit def show[A]: Show[Term[A]] = Show.showFromToString
}

sealed trait TermFunctions {
  protected def walk[A : Order](t: Term[A], sub: IMap[Term[A], Term[A]]): Term[A] =
    t match {
      case v@Term.Var(_) => sub.lookup(v).fold(t)(vt => walk(vt, sub))
      case _ => t
    }

  protected def extend[A : Order](x: Term[A], v: Term[A], sub: IMap[Term[A], Term[A]]): Maybe[IMap[Term[A], Term[A]]] =
    x match {
      case vr@Term.Var(_) => Maybe.just(sub.insert(vr, v))
      case _ => Maybe.empty
    }
}
