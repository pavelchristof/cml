package CML.Algebra

import shapeless.Nat
import shapeless.ops.nat.ToInt

import scalaz.syntax.applicative._
import scalaz.{Applicative, Functor}

object Sized {
  def apply(size: Int): Sized[Nat] =
    new Sized()(new ToInt[Nat] { def apply(): Int = size })
}

case class Sized[S <: Nat] (implicit size: ToInt[S]) {
  implicit object Vector extends Functor[Vector] with Applicative[Vector] with Additive1[Vector] {
    override def map[A, B](fa: Vector[A])(f: (A) => B): Vector[B] = Vector[B](fa.vec.map(f))

    override def point[A](a: => A): Vector[A] = Vector(scala.collection.immutable.Vector.fill(size())(a))
    override def ap[A, B](fa: => Vector[A])(f: => Vector[(A) => B]): Vector[B] =
      Vector(fa.vec.zip(f.vec).map(x => x._2(x._1)))

    override def zero[F](implicit f: Field[F]): Vector[F] = point(f.zero)
    override def add[F](x: Vector[F], y: Vector[F])(implicit f: Field[F]): Vector[F] = ^(x, y) (f.add)
    override def sub[F](x: Vector[F], y: Vector[F])(implicit f: Field[F]): Vector[F] = ^(x, y) (f.sub)
    override def neg[F](x: Vector[F])(implicit f: Field[F]): Vector[F] = map(x)(f.neg)
  }

  case class Vector[A] (
    vec: scala.collection.immutable.Vector[A]
  ) {
    override def toString() = vec.toString()
  }
}
