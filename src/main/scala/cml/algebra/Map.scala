package cml.algebra

import cml.Enumerate
import cml.algebra.traits._
import scalaz.Monoid
import scalaz.Scalaz._

object Map {
  class MapAdditive1[K] extends Additive1[({type T[A] = Map[K, A]})#T] {
    override def zero[F](implicit f: Additive[F]): Map[K, F] = collection.immutable.Map.empty
    override def add[F](x: Map[K, F], y: Map[K, F])(implicit f: Additive[F]): Map[K, F] = x.unionWith(y)(f.add)
    override def sub[F](x: Map[K, F], y: Map[K, F])(implicit f: Additive[F]): Map[K, F] = x.unionWith(y)(f.sub)
    override def neg[F](x: Map[K, F])(implicit f: Additive[F]): Map[K, F] = x.mapValues(f.neg)
  }

  class MapLinear[K] extends MapAdditive1[K] with Linear[({type T[A] = Map[K, A]})#T] {
    override def mull[F](a: F, v: Map[K, F])(implicit f: Field[F]): Map[K, F] = v.mapValues(f.mul(a, _))
    override def mulr[F](v: Map[K, F], a: F)(implicit f: Field[F]): Map[K, F] = v.mapValues(f.mul(_, a))
    override def div[F](v: Map[K, F], a: F)(implicit f: Field[F]): Map[K, F] = v.mapValues(f.div(_, a))
  }

  class MapNormed[K] extends MapLinear[K] with Normed[({type T[A] = Map[K, A]})#T] {
    override def sum[F](v: Map[K, F])(implicit f: Additive[F]): F = v.foldLeft(f.zero){case (x, (_, y)) => f.add(x, y)}
    override def dot[F](u: Map[K, F], v: Map[K, F])(implicit f: Field[F]): F = sum(u.intersectWith(v)(f.mul))
    override def taxicab[F](v: Map[K, F])(implicit f: Analytic[F]): F = sum(v.mapValues(f.abs))
  }

  class MapLocallyConcrete[K] (implicit e: Enumerate[K]) extends MapNormed[K] with LocallyConcrete[({type T[A] = Map[K, A]})#T] {
    override type Index = K

    /**
     * The index must be recursively enumerable.
     */
    override def enumerateIndex: Enumerate[Index] = e

    /**
     * The (normal) basis for this vector space.
     */
    override def basis[A](i: Index)(implicit field: Field[A]): Map[K, A] =
      collection.immutable.Map(i -> field.one)

    /**
     * Find the coefficient of a basis vector.
     */
    override def indexLC[A](v: Map[K, A])(i: Index)(implicit a: Additive[A]): A =
      v.applyOrElse(i, (_: K) => a.zero)

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulateLC[A](h: Map[Index, A])(implicit a: Additive[A]): Map[K, A] =
      h

    /**
     * Maps the vector with a function f. It must hold that f(0) = 0.
     */
    override def mapLC[A, B](x: Map[K, A])(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): Map[K, B] =
      x.mapValues(f)

    /**
     * Applies a vector of functions to a vector, pointwise. It must hold for each function that f(0) = 0.
     */
    override def apLC[A, B](x: Map[K, A])(f: Map[K, (A) => B])(implicit a: Additive[A], b: Additive[B]): Map[K, B] =
      x.intersectWith(f){ case (y, g) => g(y) }

    /**
     * Applies a binary function pointwise. If must hold that f(0, 0) = 0.
     */
    override def apply2LC[A, B, C](x: Map[K, A], y: Map[K, B])(f: (A, B) => C)
        (implicit a: Additive[A], b: Additive[B], c: Additive[C]): Map[K, C] =
      x.intersectWith(y)(f)

    /**
     * Returns the concrete subspace containing v.
     */
    override def restrict[A](v: Map[K, A])(implicit field: Field[A]): Subspace[({type T[A] = Map[K, A]})#T] =
      MapSubspace[K](v.keySet)

    override def restrict[A](h: Map[K, A] => A)(v: Map[K, A])
        (implicit a: Additive[A]): Subspace[({type T[A] = Map[K, A]})#T] = {
      val context = Context[K](iterated = false, collection.mutable.Set())
      h(SpyMap(context, collection.immutable.Map()))
      var keys = context.accessed
      if (context.iterated)
        keys ++= v.keySet
      MapSubspace[K](keys.toSet)
    }
  }

  // TODO: optimize this!
  case class MapSubspace[K] (keys: Set[K]) extends Subspace[({type T[A] = Map[K, A]})#T] {
    override type Type[A] = Map[K, A]

    override def project[A](v: Map[K, A])(implicit a: Additive[A]): Map[K, A] =
      keys.map(k => (k, v.applyOrElse(k, (_: K) => a.zero))).toMap
    override def inject[A](u: Map[K, A])(implicit a: Additive[A]): Map[K, A] =
      u

    override implicit val concrete: Concrete[Type] = new SubMap[K](keys.toSet)
  }

  case class Context[K] (
    var iterated: Boolean,
    accessed: collection.mutable.Set[K]
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

  def additive1[K] = new MapAdditive1[K]
  def linear[K] = new MapLinear[K]
  def normed[K] = new MapNormed[K]
  def locallyConcrete[K](implicit e: Enumerate[K]) =
    new MapLocallyConcrete[K]
}
