package cml.algebra

import cml.Enumerate
import cml.algebra.traits._
import scala.collection.immutable.HashMap
import scalaz.Monoid
import scalaz.Scalaz._

object MapInstances {
  def unionWith[K, V](x: Map[K, V], y: Map[K, V])(f: (V, V) => V): Map[K, V] = {
    def merge(u: (K, V), v: (K, V)): (K, V) = (u._1, f(u._2, v._2))
    x.unionWith(y)(f)
  }

  def intersectWith[K, A, B, C](x: Map[K, A], y: Map[K, B])(f: (A, B) => C): Map[K, C] = {
    if (x.size <= y.size) {
      var b = HashMap.newBuilder[K, C]
      for (kv <- x) {
        y.get(kv._1) match {
          case Some(v) => b += kv._1 -> f(kv._2, v)
          case None => ()
        }
      }
      b.result()
    } else {
      def swapped(u: B, v: A): C = f(v, u)
      intersectWith(y, x)(swapped)
    }
  }

  class MapAdditive1[K] extends Additive1[({type T[A] = Map[K, A]})#T] {
    override def zero[F](implicit f: Additive[F]): Map[K, F] =
      HashMap()
    override def add[F](x: Map[K, F], y: Map[K, F])(implicit f: Additive[F]): Map[K, F] =
      unionWith(x, y)(f.add)
    override def sub[F](x: Map[K, F], y: Map[K, F])(implicit f: Additive[F]): Map[K, F] =
      unionWith(x, y)(f.sub)
    override def neg[F](x: Map[K, F])(implicit f: Additive[F]): Map[K, F] =
      x.map(kv => (kv._1, f.neg(kv._2)))
  }

  class MapLinear[K] extends MapAdditive1[K] with Linear[({type T[A] = Map[K, A]})#T] {
    override def mull[F](a: F, v: Map[K, F])(implicit f: Field[F]): Map[K, F] =
      v.map(kv => (kv._1, f.mul(a, kv._2)))
    override def mulr[F](v: Map[K, F], a: F)(implicit f: Field[F]): Map[K, F] =
      v.map(kv => (kv._1, f.mul(kv._2, a)))
    override def div[F](v: Map[K, F], a: F)(implicit f: Field[F]): Map[K, F] =
      v.map(kv => (kv._1, f.div(kv._2, a)))
  }

  class MapLocallyConcrete[K] (implicit e: Enumerate[K])
    extends MapLinear[K] with LocallyConcrete[({type T[A] = Map[K, A]})#T] {
    override type Index = K

    /**
     * The index must be recursively enumerable.
     */
    override def enumerateIndex: Enumerate[Index] = e

    /**
     * The (normal) basis for this vector space.
     */
    override def basis[A](i: Index)(implicit field: Field[A]): Map[K, A] =
      HashMap(i -> field.one)

    /**
     * Find the coefficient of a basis vector.
     */
    override def indexLC[A](v: Map[K, A])(i: Index)(implicit a: Additive[A]): A =
      v.applyOrElse(i, (_: K) => a.zero)

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulateLC[A](h: Map[K, A])(implicit a: Additive[A]): Map[K, A] =
      HashMap(h.toSeq: _*)

    /**
     * Maps the vector with a function f. It must hold that f(0) = 0.
     */
    override def map[A, B](x: Map[K, A])(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): Map[K, B] =
      x.map(kv => (kv._1, f(kv._2)))

    /**
     * Applies a vector of functions to a vector, pointwise. It must hold for each function that f(0) = 0.
     */
    override def ap[A, B](x: Map[K, A])(f: Map[K, (A) => B])(implicit a: Additive[A], b: Additive[B]): Map[K, B] =
      intersectWith(x, f){ case (y, g) => g(y) }

    /**
     * Applies a binary function pointwise. If must hold that f(0, 0) = 0.
     */
    override def apply2[A, B, C](x: Map[K, A], y: Map[K, B])(f: (A, B) => C)
        (implicit a: Additive[A], b: Additive[B], c: Additive[C]): Map[K, C] =
      intersectWith(x, y)(f)

    /**
     * Returns a concrete subspace restricted to a set of keys.
     */
    override def restrict(keys: Set[K]): Subspace[({type T[A] = Map[K, A]})#T] =
      MapSubspace[K](keys.zipWithIndex.toMap)
  }

  class MapNormed[K] (implicit e: Enumerate[K]) extends MapLocallyConcrete[K] with Normed[({type T[A] = Map[K, A]})#T] {
    override def sum[F](v: Map[K, F])(implicit f: Additive[F]): F =
      v.values.fold[F](f.zero)(f.add(_, _))
  }

  case class MapSubspace[K] (keyMap: Map[K, Int]) extends Subspace[({type T[A] = Map[K, A]})#T] {
    val sizeNat = RuntimeNat(keyMap.size)
    val space = Vector(sizeNat())

    override type Type[A] = space.Type[A]
    override implicit val concrete: Concrete[Type] = space

    override def project[A](v: Map[K, A])(implicit a: Additive[A]): Type[A] = {
      val arr = new Array[A](keyMap.size)
      for ((k, i) <- keyMap) {
        arr(i) = v.getOrElse(k, a.zero)
      }
      Vector(arr)
    }

    override def inject[A](u: Type[A])(implicit a: Additive[A]): Map[K, A] = new Map[K, A] {
      override def +[B1 >: A](kv: (K, B1)): Map[K, B1] = throw new UnsupportedOperationException()
      override def -(key: K): Map[K, A] =  throw new UnsupportedOperationException()

      override def get(key: K): Option[A] = Some(u.get(keyMap(key)))

      override def iterator: Iterator[(K, A)] = keyMap.iterator.map(ki => (ki._1, u.get(ki._2)))
    }
  }

  def additive1[K] = new MapAdditive1[K]
  def linear[K] = new MapLinear[K]
  def locallyConcrete[K](implicit e: Enumerate[K]) =
    new MapLocallyConcrete[K]
  def normed[K](implicit e: Enumerate[K]) = new MapNormed[K]
}
