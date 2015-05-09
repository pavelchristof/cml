package CML.Algebra

import shapeless.Nat
import shapeless.ops.nat.ToInt

import scalaz.{Applicative, Zip}

case class Vector[S <: Nat, A] (
  val vec: scala.collection.immutable.Vector[A]
) extends Traversable[A]  {
  override def foreach[U](f: (A) => U): Unit = vec.foreach(f)
}

class VectorImpl[S <: Nat](size: S)(implicit n: ToInt[S])
  extends Zip[({type λ[α] = Vector[S, α]})#λ]
  with Applicative[({type λ[α] = Vector[S, α]})#λ]
  with Concrete[({type λ[α] = Vector[S, α]})#λ] {
  override def zip[A, B](a: => Vector[S, A], b: => Vector[S, B]): Vector[S, (A, B)] = new Vector(a.vec.zip(b.vec))

  override def map[A, B](fa: Vector[S, A])(f: (A) => B): Vector[S, B] = Vector[S, B](fa.vec.map(f))
  override def point[A](a: => A): Vector[S, A] = new Vector(scala.collection.immutable.Vector.fill(dim)(a))
  override def ap[A, B](fa: => Vector[S, A])(f: => Vector[S, (A) => B]): Vector[S, B] =
    map(zip(fa, f))(x => x._2(x._1))

  override def zero[F](implicit f: Field[F]): Vector[S, F] = point(f.zero)
  override def add[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Field[F]): Vector[S, F] =
    map(zip(x, y))(x => f.add(x._1, x._2))
  override def sub[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Field[F]): Vector[S, F] =
    map(zip(x, y))(x => f.sub(x._1, x._2))
  override def neg[F](x: Vector[S, F])(implicit f: Field[F]): Vector[S, F] = map(x)(f.neg)

  override def mull[F](a: F, v: Vector[S, F])(implicit f: Field[F]): Vector[S, F] = map(v)(f.mul(a, _))
  override def mulr[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = map(v)(f.mul(_, a))
  override def div[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = map(v)(f.div(_, a))

  override def taxicab[F](v: Vector[S, F])(implicit f: Analytic[F]): F = v.map(f.abs).fold(f.zero)(f.add)
  override def length[F](v: Vector[S, F])(implicit f: Analytic[F]): F = f.sqrt(dot(v, v))
  override def dot[F](u: Vector[S, F], v: Vector[S, F])(implicit f: Field[F]): F =
    apply2(u, v)(f.mul).fold(f.zero)(f.add)

  override def dim: Int = n()
  override def map[F](g: AnalyticMap)(v: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = map(v)(x => g(x))
}

object Vector {
  def apply[S <: Nat](size: S)(implicit n: ToInt[S]) = new VectorImpl[S](size)(n)
}
