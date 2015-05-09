package CML.Algebra

import shapeless.Nat
import shapeless.ops.nat.ToInt

import scalaz.{Applicative, Zip}

case class Vector[S <: Nat, A] (
  val vec: scala.collection.immutable.Vector[A]
) extends Traversable[A]  {
  override def foreach[U](f: (A) => U): Unit = vec.foreach(f)
}

object Vector {
  implicit def zip[S <: Nat] = new Zip[({type λ[α] = Vector[S, α]})#λ] {
    override def zip[A, B](a: => Vector[S, A], b: => Vector[S, B]) = Vector(a.vec.zip(b.vec))
  }

  implicit def applicative[S <: Nat](implicit size: ToInt[S]) = new Applicative[({type λ[α] = Vector[S, α]})#λ] {
    override def map[A, B](fa: Vector[S, A])(f: (A) => B): Vector[S, B] = Vector[S, B](fa.vec.map(f))
    override def point[A](a: => A): Vector[S, A] = Vector(scala.collection.immutable.Vector.fill(size())(a))
    override def ap[A, B](fa: => Vector[S, A])(f: => Vector[S, (A) => B]): Vector[S, B] =
      map(zip.zip(fa, f))(x => x._2(x._1))
  }

  implicit def additive1[S <: Nat](implicit size: ToInt[S]) = new Banach[({type λ[α] = Vector[S, α]})#λ] {
    override def zero[F](implicit f: Field[F]): Vector[S, F] = applicative.point(f.zero)
    override def add[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Field[F]): Vector[S, F] =
      applicative.map(zip.zip(x, y))(x => f.add(x._1, x._2))
    override def sub[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Field[F]): Vector[S, F] =
      applicative.map(zip.zip(x, y))(x => f.sub(x._1, x._2))
    override def neg[F](x: Vector[S, F])(implicit f: Field[F]): Vector[S, F] = applicative.map(x)(f.neg)

    override def mull[F](a: F, v: Vector[S, F])(implicit f: Field[F]): Vector[S, F] = applicative.map(v)(f.mul(a, _))
    override def mulr[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = applicative.map(v)(f.mul(_, a))
    override def div[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = applicative.map(v)(f.div(_, a))

    override def taxicab[F](v: Vector[S, F])(implicit f: Analytic[F]): F = v.map(f.abs).fold(f.zero)(f.add)
    override def length[F](v: Vector[S, F])(implicit f: Analytic[F]): F = f.sqrt(dot(v, v))
    override def dot[F](u: Vector[S, F], v: Vector[S, F])(implicit f: Field[F]): F =
      applicative.apply2(u, v)(f.mul).fold(f.zero)(f.add)

    override def exp[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.exp)
    override def acos[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.acos)
    override def atan[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.atan)
    override def tanh[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.tanh)
    override def log[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.log)
    override def cosh[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.cosh)
    override def tan[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.tan)
    override def cos[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.cos)
    override def asin[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.asin)
    override def sqrt[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.sqrt)
    override def sin[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.sin)
    override def sinh[F](x: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = applicative.map(x)(f.sinh)
  }
}
