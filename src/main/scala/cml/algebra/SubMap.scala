package cml.algebra

import cml.Enumerate
import cml.algebra.traits._

import scalaz.Monoid
import scalaz.Scalaz._

class SubMap[K](keys: Set[K]) extends Concrete[({type T[A] = Map[K, A]})#T] {
  override type Index = K

  /**
   * The (finite) dimension of this vector space.
   */
  val dimFin: BigInt = keys.size

  /**
   * The index must be recursively enumerable.
   */
  override def enumerateIndex: Enumerate[Index] = Enumerate.seq(keys.toSeq)

  /**
   * Construct a vector from coefficients of the basis vectors.
   */
  override def tabulate[A](h: (Index) => A): Map[K, A] = keys.map(k => (k, h(k))).toMap

  /**
   * Find the coefficient of a basis vector.
   */
  override def index[A](v: Map[K, A])(i: Index): A = v(i)

  override def map[A, B](x: Map[K, A])(f: (A) => B): Map[K, B] = x.mapValues(f)

  override def point[A](a: => A): Map[K, A] = tabulate(_ => a)

  override def ap[A, B](x: => Map[K, A])(f: => Map[K, (A) => B]): Map[K, B] =
    x.intersectWith(f){ case (y, g) => g(y) }

  override def foldMap[A, B](v: Map[K, A])(op: (A) => B)(implicit m: Monoid[B]): B =
    v.mapValues(op).values.fold(m.zero)(m.append(_, _))

  override def foldRight[A, B](v: Map[K, A], z: => B)(op: (A, => B) => B): B =
    v.values.foldRight(z)(op(_, _))
}
