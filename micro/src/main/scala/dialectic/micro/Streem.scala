package dialectic.micro

import scalaz.{ Foldable, IList, Monad }

/** A list-and-stream-eque data structure.
  *
  * A [[dialectic.micro.Streem]] is either empty, an evaluated head and tail,
  * or a trampolined [[dialectic.micro.Streem]].
  */
sealed abstract class Streem[A] {
  import Streem._

  def ++(other: => Streem[A]): Streem[A] =
    fold(other,
         (h, t) => Mature(h, t.merge(other)),
         s => Immature(() => s.merge(other)))

  def flatMap[B](f: A => Streem[B]): Streem[B] =
    fold(Empty(),
         (h, t) => f(h) ++ t.flatMap(f),
         s => Immature(() => s.flatMap(f)))

  def fold[B](empty: => B, mature: (A, Streem[A]) => B, immature: (=> Streem[A]) => B): B =
    this match {
      case Empty() => empty
      case Mature(h, t) => mature(h, t)
      case Immature(s) => immature(s())
    }

  def map[B](f: A => B): Streem[B] =
    flatMap(a => Streem.single(f(a)))

  /** Merge this and `other`, switching between the two every so often. */
  def merge(other: Streem[A]): Streem[A] =
    fold(other,
         (h, t) => Mature(h, t.merge(other)),
         s => Immature(() => other.merge(s)))

  /** Similar to `flatMap` but `merge`s intermediary streams instead of simply appending them. */
  def mergeMap[B](f: A => Streem[B]): Streem[B] =
    fold(Empty(),
         (h, t) => f(h).merge(t.mergeMap(f)),
         s => Immature(() => s.mergeMap(f)))

  def take(n: Int): Streem[A] =
    if (n == 0) Empty()
    else
      fold(Empty(),
           (h, t) => Mature(h, t.take(n - 1)),
           _.take(n))

  def takeAll: IList[A] =
    fold(IList.empty, _ :: _.takeAll, _.takeAll)
}

object Streem extends StreemInstances {
  /** Similar to empty list */
  final case class Empty[A]() extends Streem[A]

  /** Similar to list cons */
  final case class Mature[A](head: A, tail: Streem[A]) extends Streem[A]

  /** Trampolined stream */
  final case class Immature[A](stream: () => Streem[A]) extends Streem[A]

  /** Convenience constructor for [[dialectic.micro.Streem.Empty]]. */
  def empty[A]: Streem[A] = Empty()

  /** Convenience constructor for [[dialectic.micro.Streem.Mature]]. */
  def cons[A](head: A, tail: Streem[A]): Streem[A] = Mature(head, tail)

  /** Convenience constructor for [[dialectic.micro.Streem.Immature]]. */
  def delay[A](s: => Streem[A]): Streem[A] = Immature(() => s)

  def single[A](a: A): Streem[A] = cons(a, empty)

  def apply[A](as: A*): Streem[A] =
    as.foldRight(empty[A])(cons)

  /** `Foldable` is ~ `toList`, and any list can be made a `[[dialectic.micro.Streem]]`. */
  def fromFoldable[F[_] : Foldable, A](fa: F[A]): Streem[A] =
    Foldable[F].foldRight(fa, empty[A]) { case (h, t) => cons(h, t) }
}

sealed abstract class StreemInstances {
  implicit val instances: Monad[Streem] =
    new Monad[Streem] {
      def bind[A, B](fa: Streem[A])(f: A => Streem[B]): Streem[B] = fa.flatMap(f)

      def point[A](a: => A): Streem[A] = Streem.single(a)
    }
}
