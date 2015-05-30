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

  class MapNormed[K] extends MapLinear[K] with Normed[({type T[A] = Map[K, A]})#T] {
    override def sum[F](v: Map[K, F])(implicit f: Additive[F]): F =
      v.foldLeft(f.zero){case (x, (_, y)) => f.add(x, y)}
    override def dot[F](u: Map[K, F], v: Map[K, F])(implicit f: Field[F]): F =
      sum(intersectWith(u, v)(f.mul))
    override def taxicab[F](v: Map[K, F])(implicit f: Analytic[F]): F =
      sum(v.map(kv => (kv._1, f.abs(kv._2))))
  }

  class MapLocallyConcrete[K] (implicit e: Enumerate[K]) 
    extends MapNormed[K] with LocallyConcrete[({type T[A] = Map[K, A]})#T] {
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
    override def mapLC[A, B](x: Map[K, A])(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): Map[K, B] =
      x.map(kv => (kv._1, f(kv._2)))

    /**
     * Applies a vector of functions to a vector, pointwise. It must hold for each function that f(0) = 0.
     */
    override def apLC[A, B](x: Map[K, A])(f: Map[K, (A) => B])(implicit a: Additive[A], b: Additive[B]): Map[K, B] =
      intersectWith(x, f){ case (y, g) => g(y) }

    /**
     * Applies a binary function pointwise. If must hold that f(0, 0) = 0.
     */
    override def apply2LC[A, B, C](x: Map[K, A], y: Map[K, B])(f: (A, B) => C)
        (implicit a: Additive[A], b: Additive[B], c: Additive[C]): Map[K, C] =
      intersectWith(x, y)(f)

    /**
     * Returns the concrete subspace containing v.
     */
    override def restrict[A](v: Map[K, A])(implicit field: Field[A]): Subspace[({type T[A] = Map[K, A]})#T] =
      MapSubspace[K](v.keySet)

    override def restrict[A](h: Map[K, A] => A)(v: Map[K, A])
        (implicit a: Additive[A]): Subspace[({type T[A] = Map[K, A]})#T] = {
      val context = Context[K](iterated = false, collection.mutable.HashSet())
      h(SpyMap(context, HashMap()))
      var keys = context.accessed
      if (context.iterated)
        keys ++= v.keySet
      MapSubspace[K](keys.toSet)
    }
  }

  // TODO: perhaps use an array as the subspace?
  case class MapSubspace[K] (keys: Set[K]) extends Subspace[({type T[A] = Map[K, A]})#T] {
    override type Type[A] = Map[K, A]

    override def project[A](v: Map[K, A])(implicit a: Additive[A]): Map[K, A] = {
      val b = HashMap.newBuilder[K, A]
      for (k <- keys) {
        b += k -> v.getOrElse(k, a.zero)
      }
      b.result()
    }
    override def inject[A](u: Map[K, A])(implicit a: Additive[A]): Map[K, A] =
      u

    override implicit val concrete: Concrete[Type] = new SubMap[K](keys.toSet)
  }

  case class Context[K] (
    var iterated: Boolean,
    accessed: collection.mutable.HashSet[K]
  )

  case class SpyMap[K, A] (
    context: Context[K],
    content: Map[K, A]
    ) extends Map[K, A] {
    override def +[B1 >: A](kv: (K, B1)): Map[K, B1] = SpyMap(context, content + kv)
    override def -(key: K): Map[K, A] = SpyMap(context, content - key)

    override def get(key: K): Option[A] = {
      context.accessed += key
      content.get(key)
    }

    override def iterator: Iterator[(K, A)] = {
      context.iterated = true
      content.iterator
    }
  }

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
    override def tabulate[A](h: (Index) => A): Map[K, A] = {
      val b = HashMap.newBuilder[K, A]
      for (k <- keys) {
        b += k -> h(k)
      }
      b.result()
    }

    /**
     * Find the coefficient of a basis vector.
     */
    override def index[A](v: Map[K, A])(i: Index): A =
      v(i)

    override def map[A, B](x: Map[K, A])(f: (A) => B): Map[K, B] =
      x.map(kv => (kv._1, f(kv._2)))

    override def point[A](a: => A): Map[K, A] =
      tabulate(_ => a)

    override def ap[A, B](x: => Map[K, A])(f: => Map[K, (A) => B]): Map[K, B] =
      intersectWith(x, f){ case (y, g) => g(y) }

    override def foldMap[A, B](v: Map[K, A])(op: (A) => B)(implicit m: Monoid[B]): B =
      v.map(kv => op(kv._2)).fold(m.zero)(m.append(_, _))

    override def foldRight[A, B](v: Map[K, A], z: => B)(op: (A, => B) => B): B =
      v.values.foldRight(z)(op(_, _))
  }

  def additive1[K] = new MapAdditive1[K]
  def linear[K] = new MapLinear[K]
  def normed[K] = new MapNormed[K]
  def locallyConcrete[K](implicit e: Enumerate[K]) =
    new MapLocallyConcrete[K]
}
