package cml.algebra

import cml.Enumerate
import cml.algebra.traits._
import shapeless.Nat
import shapeless.ops.nat.ToInt
import scala.collection.mutable
import scala.reflect.ClassTag
import scalaz.std.AllInstances._
import scalaz.{Monoid, Traverse, Applicative, Zip}

case class Vector[+S <: Nat, A] (
  get: Array[A]
) extends Serializable {
  override def toString: String = get.toVector.toString()
}

class VectorImpl[S <: Nat](implicit size: ToInt[S])
  extends Concrete[({type `T[S]`[a] = Vector[S, a]})#`T[S]`]
  with Serializable {

  type Type[a] = Vector[S, a]

  /**
   * Constructs a vector from a sequence.
   */
  def from[A : ClassTag](vec: Seq[A]): Option[Vector[S, A]] =
    if (vec.size == size()) {
      Some(Vector(vec.toArray))
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
    v.get(i)

  /**
   * Construct a vector using given coefficients for the orthonormal basis.
   */
  override def tabulate[F](f: (Int) => F)(implicit additive: Additive[F]): Vector[S, F] =
    Vector(Array.tabulate(size())(f(_)))

  override def map[A, B](fa: Vector[S, A])(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): Vector[S, B] = {
    val n = size()
    val arr = new Array[B](n)
    var i = 0

    while (i < n) {
      arr(i) = f(fa.get(i))
      i += 1
    }

    Vector(arr)
  }

  /**
   * Applies a vector of functions to a vector, pointwise. It must hold for each function that f(0) = 0.
   */
  override def ap[A, B](x: Vector[S, A])(f: Vector[S, (A) => B])
      (implicit a: Additive[A], b: Additive[B]): Vector[S, B] = {
    val n = size()
    val arr = new Array[B](n)
    var i = 0

    while (i < n) {
      arr(i) = f.get(i)(x.get(i))
      i += 1
    }

    Vector(arr)
  }

  /**
   * Applies a binary function pointwise. It must hold that f(0, 0) = 0.
   */
  override def apply2[A, B, C](x: Vector[S, A], y: Vector[S, B])(f: (A, B) => C)
      (implicit a: Additive[A], b: Additive[B], c: Additive[C]): Vector[S, C] = {
    val n = size()
    val arr = new Array[C](n)
    var i = 0

    while (i < n) {
      arr(i) = f(x.get(i), y.get(i))
      i += 1
    }

    Vector(arr)
  }

  override def sum[F](v: Vector[S, F])(implicit f: Additive[F]): F
    = v.get.fold(f.zero)(f.add(_, _))

  override def point[A](a: => A)(implicit additive: Additive[A]): Vector[S, A] =
    Vector(Array.fill(size())(a))
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
