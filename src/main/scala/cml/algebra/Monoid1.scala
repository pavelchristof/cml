package cml.algebra

import scalaz.Monoid

trait Monoid1[F[_]] extends Serializable {
  def empty[A](implicit a: AbelianRing[A]): F[A]
  def append[A](x: F[A], y: F[A])(implicit a: AbelianRing[A]): F[A]
}

object Monoid1 {
  import Additive1.asAdditive

  class AsMonoid[F[_], A](implicit monoid1: Monoid1[F], a: AbelianRing[A]) extends Monoid[F[A]] with Serializable {
    override def zero: F[A] = monoid1.empty
    override def append(f1: F[A], f2: => F[A]): F[A] = monoid1.append(f1, f2)
  }

  implicit def asMonoid[F[_], A](implicit monoid1: Monoid1[F], a: AbelianRing[A]) = new AsMonoid[F, A]()

  class Matrix[F[_]] (
    implicit space: Normed[F]
  ) extends Monoid1[({type T[A] = F[F[A]]})#T] {
    override def empty[A](implicit a: AbelianRing[A]): F[F[A]] =
      space.tabulate(i => space.tabulate(j => if (i == j) a.one else a.zero))
    override def append[A](x: F[F[A]], y: F[F[A]])(implicit a: AbelianRing[A]): F[F[A]] =
      space.map(x)(g => space.sum(space.apply2(g, y)(space.mull(_, _))))
  }

  def matrix[F[_]](implicit space: Normed[F]) = new Matrix[F]
}
