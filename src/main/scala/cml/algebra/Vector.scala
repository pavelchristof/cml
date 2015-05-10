package cml.algebra

import cml.algebra.traits._
import shapeless.Nat
import shapeless.ops.nat.ToInt
import scalaz.std.AllInstances._
import scalaz.{Traverse, Applicative, Zip}

case class Vector[+S <: Nat, +A] (
  vec: scala.collection.immutable.Vector[A]
) extends Serializable

class VectorImpl[S <: Nat](implicit size: ToInt[S])
  extends Zip[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Applicative[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Traverse[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Concrete[({type `T[S]`[a] = Vector[S, a]})#`T[S]`] {
  type Type[a] = Vector[S, a]

  def from[A](vec: Seq[A]): Option[Vector[S, A]] =
    if (vec.size == dim) {
      Some(Vector(vec.toVector))
    } else {
      None
    }

  import traverseSyntax._

  override def zip[A, B](a: => Vector[S, A], b: => Vector[S, B]): Vector[S, (A, B)] = new Vector(a.vec.zip(b.vec))

  override def map[A, B](fa: Vector[S, A])(f: (A) => B): Vector[S, B] = Vector[S, B](fa.vec.map(f))
  override def point[A](a: => A): Vector[S, A] = new Vector(scala.collection.immutable.Vector.fill(dim)(a))
  override def ap[A, B](fa: => Vector[S, A])(f: => Vector[S, (A) => B]): Vector[S, B] =
    zip(fa, f).map(x => x._2(x._1))
  override def traverseImpl[G[_], A, B](fa: Vector[S, A])(f: (A) => G[B])
      (implicit g: Applicative[G]): G[Vector[S, B]] =
    g.map(implicitly[Traverse[scala.collection.immutable.Vector]].traverse(fa.vec)(f))(Vector(_))

  override def zero[F](implicit f: Field[F]): Vector[S, F] = point(f.zero)
  override def add[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Field[F]): Vector[S, F] =
    zip(x, y).map(x => f.add(x._1, x._2))
  override def sub[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Field[F]): Vector[S, F] =
    zip(x, y).map(x => f.sub(x._1, x._2))
  override def neg[F](x: Vector[S, F])(implicit f: Field[F]): Vector[S, F] = x.map(f.neg)

  override def mull[F](a: F, v: Vector[S, F])(implicit f: Field[F]): Vector[S, F] = v.map(f.mul(a, _))
  override def mulr[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = v.map(f.mul(_, a))
  override def div[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = v.map(f.div(_, a))

  override def taxicab[F](v: Vector[S, F])(implicit f: Analytic[F]): F = v.map(f.abs).foldLeft(f.zero)(f.add)
  override def length[F](v: Vector[S, F])(implicit f: Analytic[F]): F = f.sqrt(dot(v, v))
  override def dot[F](u: Vector[S, F], v: Vector[S, F])(implicit f: Field[F]): F =
    apply2(u, v)(f.mul).foldLeft(f.zero)(f.add)

  override def dim: Int = size()
  override def pointwise[F](g: AnalyticMap)(v: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = v.map(g(_))
}

object Vector {
  def apply[S <: Nat](implicit size: ToInt[S]) =
    new VectorImpl[S]

  def apply[S <: Nat](n: S)(implicit size: ToInt[S]) =
    new VectorImpl[S]
}
