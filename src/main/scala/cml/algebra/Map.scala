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
     * Returns the concrete subspace containing v.
     */
    override def restrict[A](v: Map[K, A])(implicit field: Field[A]): Concrete[({type T[A] = Map[K, A]})#T] =
      new SubMap[K](v.keySet)

    /**
     * The fundamental property of locally concrete vector spaces is that for any function f on vectors polymorphic in
     * the number type and for each vector v in V, we can factor V as X x Y where X is concrete and f(v) = f(v + y) for
     * all y in Y. This function finds such a subspace X, not necessarily the smallest.
     *
     * It follows that the derivative df(x)/dy = 0 for any y in Y. As such it is enough to consider partial derivatives
     * on X to find the gradient of f.
     *
     * The subspace X does not always depend on the vector v. It only depends on v (and contains restrict(v)) when the
     * function f uses accumulating functions such as sum(), length(), etc. Otherwise the subspace X is constant for
     * all v in V.
     */
    override def restrict[A](h: Map[K, A] => A)(v: Map[K, A])
        (implicit a: Additive[A]): Concrete[({type T[A] = Map[K, A]})#T] = {
      val context = Context[K](false, collection.mutable.Set())
      h(SpyMap(context, collection.immutable.Map()))
      var keys = context.accessed
      if (context.iterated)
        keys ++= v.keySet
      new SubMap[K](keys.toSet)
    }
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
