package cml.algebra

import cml.Enumerate
import cml.algebra.traits._
import shapeless.Nat
import shapeless.ops.nat.ToInt
import scalaz.std.AllInstances._
import scalaz.{Monoid, Traverse, Applicative, Zip}

case class Vector[+S <: Nat, +A] (
  vec: scala.collection.immutable.Vector[A]
) extends Serializable

class VectorImpl[S <: Nat](implicit size: ToInt[S])
  extends Zip[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Applicative[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Traverse[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Concrete[({type `T[S]`[a] = Vector[S, a]})#`T[S]`] {

  type Type[a] = Vector[S, a]

  /**
   * Constructs a vector from a sequence.
   */
  def from[A](vec: Seq[A]): Option[Vector[S, A]] =
    if (vec.size == size()) {
      Some(Vector(vec.toVector))
    } else {
      None
    }

  import traverseSyntax._

  override def zip[A, B](a: => Vector[S, A], b: => Vector[S, B]): Vector[S, (A, B)] = new Vector(a.vec.zip(b.vec))

  override def map[A, B](fa: Vector[S, A])(f: (A) => B): Vector[S, B] = Vector[S, B](fa.vec.map(f))
  override def point[A](a: => A): Vector[S, A] = new Vector(scala.collection.immutable.Vector.fill(size())(a))
  override def ap[A, B](fa: => Vector[S, A])(f: => Vector[S, (A) => B]): Vector[S, B] =
    zip(fa, f).map(x => x._2(x._1))
  override def traverseImpl[G[_], A, B](fa: Vector[S, A])(f: (A) => G[B])
      (implicit g: Applicative[G]): G[Vector[S, B]] =
    g.map(implicitly[Traverse[scala.collection.immutable.Vector]].traverse(fa.vec)(f))(Vector(_))

  override def zero[F](implicit f: Additive[F]): Vector[S, F] = point(f.zero)
  override def add[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Additive[F]): Vector[S, F] =
    zip(x, y).map(z => f.add(z._1, z._2))
  override def sub[F](x: Vector[S, F], y: Vector[S, F])(implicit f: Additive[F]): Vector[S, F] =
    zip(x, y).map(z => f.sub(z._1, z._2))
  override def neg[F](x: Vector[S, F])(implicit f: Additive[F]): Vector[S, F] = x.map(f.neg)

  override def mull[F](a: F, v: Vector[S, F])(implicit f: Field[F]): Vector[S, F] = v.map(f.mul(a, _))
  override def mulr[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = v.map(f.mul(_, a))
  override def div[F](v: Vector[S, F], a: F)(implicit f: Field[F]): Vector[S, F] = v.map(f.div(_, a))

  override def taxicab[F](v: Vector[S, F])(implicit f: Analytic[F]): F = v.map(f.abs).foldLeft(f.zero)(f.add)
  override def length[F](v: Vector[S, F])(implicit f: Analytic[F]): F = f.sqrt(dot(v, v))
  override def dot[F](u: Vector[S, F], v: Vector[S, F])(implicit f: Field[F]): F =
    apply2(u, v)(f.mul).foldLeft(f.zero)(f.add)


  type Index = Int
  override def enumerateIndex: Enumerate[Int] = Enumerate.natInt(size())

  /**
   * Find the coefficient of the i-th basis vector.
   */
  override def index[F](v: Vector[S, F])(i: Int): F =
    v.vec(i)

  /**
   * Construct a vector using given coefficients for the orthonormal basis.
   */
  override def tabulate[F](f: (Int) => F): Vector[S, F] =
    Vector(collection.immutable.Vector.tabulate(size())(f(_)))

  /**
   * The dimension of this vector space.
   */
  override val dimFin: BigInt = size()

  /**
   * Applies a function pointwise on the coordinates of the vector.
   *
   * Restricted to natural maps.
   */
  override def pointwise[F](g: AnalyticMap)(v: Vector[S, F])(implicit f: Analytic[F]): Vector[S, F] = v.map(g(_))

  override def foldMap[A, B](v: Vector[S, A])(op: (A) => B)(implicit m: Monoid[B]): B =
    v.vec.map(op).fold(m.zero)(m.append(_, _))

  override def foldRight[A, B](v: Vector[S, A], z: => B)(op: (A, => B) => B): B =
    v.vec.foldRight(z)(op(_, _))
}

object Vector {
  def apply[S <: Nat](implicit size: ToInt[S]) =
    new VectorImpl[S]

  def apply[S <: Nat](n: S)(implicit size: ToInt[S]) =
    new VectorImpl[S]
}
