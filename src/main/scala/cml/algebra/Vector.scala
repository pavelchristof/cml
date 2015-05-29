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
  extends Concrete[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Traverse[({type `T[S]`[a] = Vector[S, a]})#`T[S]`] {

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

  /**
   * A countable or finite set indexing the basis.
   */
  type Index = Int

  /**
   * The index must be recursively enumerable.
   */
  override def enumerateIndex: Enumerate[Int] = Enumerate.natInt(size())

  /**
   * The dimension of this vector space.
   */
  override val dimFin: BigInt = size()

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

  override def map[A, B](fa: Vector[S, A])(f: (A) => B): Vector[S, B] = Vector[S, B](fa.vec.map(f))

  override def point[A](a: => A): Vector[S, A] = new Vector(collection.immutable.Vector.fill(size())(a))

  override def ap[A, B](fa: => Vector[S, A])(f: => Vector[S, (A) => B]): Vector[S, B] =
    Vector((fa.vec, f.vec).zipped.map{ case (x, g) => g(x) })

  override def traverseImpl[G[_], A, B](fa: Vector[S, A])(f: (A) => G[B])
      (implicit g: Applicative[G]): G[Vector[S, B]] =
    g.map(implicitly[Traverse[scala.collection.immutable.Vector]].traverse(fa.vec)(f))(Vector(_))

  override def foldMap[A, B](v: Vector[S, A])(op: (A) => B)(implicit m: Monoid[B]): B =
    v.vec.map(op).fold(m.zero)(m.append(_, _))

  override def foldRight[A, B](v: Vector[S, A], z: => B)(op: (A, => B) => B): B =
    v.vec.foldRight(z)(op(_, _))
}

trait RuntimeNat {
  type Type <: Nat
  def apply(): ToInt[Type]
}

object RuntimeNat {
  def apply(n: Int): RuntimeNat = new RuntimeNat {
    assert(n >= 0)
    override type Type = Nat
    override def apply(): ToInt[Type] = new ToInt[Type] {
      override def apply(): Int = n
    }
  }
}

object Vector {
  def apply[S <: Nat](size: ToInt[S]) =
    new VectorImpl[S]()(size)

  def apply[S <: Nat](n: S)(implicit size: ToInt[S]) =
    new VectorImpl[S]
}
