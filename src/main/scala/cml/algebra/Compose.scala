package cml.algebra

import cml.Enumerate
import cml.algebra.Compose._
import cml.algebra.traits._

import scalaz._

case class Compose[F[_], G[_]] () {
  type Type[A] = F[G[A]]

  implicit def functor(implicit f: Functor[F], g: Functor[G]): ComposeFunctor[F, G] =
    new Compose.ComposeFunctor[F, G]()
  implicit def applicative(implicit f: Applicative[F], g: Applicative[G]): ComposeApplicative[F, G] =
    new Compose.ComposeApplicative[F, G]()
  implicit def additive1(implicit f: Concrete[F], g: Additive1[G]): ComposeAdditive1[F, G] =
    new Compose.ComposeAdditive1[F, G]()
  implicit def linear(implicit f: Concrete[F], g: Linear[G]): ComposeLinear[F, G] =
    new Compose.ComposeLinear[F, G]()
  implicit def normed(implicit f: Concrete[F], g: Normed[G]): ComposeNormed[F, G] =
    new Compose.ComposeNormed
  implicit def locallyConcrete(implicit f: LocallyConcrete[F], g: LocallyConcrete[G]): ComposeLocallyConcrete[F, G] =
    new Compose.ComposeLocallyConcrete
  implicit def concrete(implicit f: Concrete[F], g: Concrete[G]): ComposeConcrete[F, G] =
    new Compose.ComposeConcrete
}

object Compose {
  class ComposeFunctor[F[_], G[_]](implicit f: Functor[F], g: Functor[G])
    extends Functor[({type T[A] = (F[G[A]])})#T] {
    override def map[A, B](v: F[G[A]])(h: (A) => B): F[G[B]] =
      f.map(v)(g.map(_)(h))
  }

  class ComposeApplicative[F[_], G[_]](implicit f: Applicative[F], g: Applicative[G])
    extends ComposeFunctor[F, G]
    with Applicative[({type T[A] = (F[G[A]])})#T] {
    override def point[A](a: => A): F[G[A]] =
      f.point(g.point(a))

    override def ap[A, B](x: => F[G[A]])(h: => F[G[(A) => B]]): F[G[B]] =
      f.ap(x)(f.map(h)(gab => g.ap(_)(gab)))
  }

  class ComposeAdditive1[F[_], G[_]](implicit f: Additive1[F], g: Additive1[G])
    extends Additive1[({type T[A] = (F[G[A]])})#T] {
    override def zero[A](implicit field: Additive[A]): F[G[A]] = f.zero[G[A]](g.additive[A])
    override def add[A](x: F[G[A]], y: F[G[A]])(implicit field: Additive[A]): F[G[A]] = f.add(x, y)(g.additive[A])
    override def neg[A](x: F[G[A]])(implicit field: Additive[A]): F[G[A]] = f.neg(x)(g.additive[A])
  }

  class ComposeLinear[F[_], G[_]](implicit f: LocallyConcrete[F], g: Linear[G])
    extends ComposeAdditive1[F, G]
    with Linear[({type T[A] = (F[G[A]])})#T] {
    override def mull[A](a: A, v: F[G[A]])(implicit field: Field[A]): F[G[A]] =
      f.mapLC(v)(g.mull(a, _))(g.additive[A], g.additive[A])
    override def mulr[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] =
      f.mapLC(v)(g.mulr(_, a))(g.additive[A], g.additive[A])
    override def div[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] =
      f.mapLC(v)(g.div(_, a))(g.additive[A], g.additive[A])
  }

  class ComposeNormed[F[_], G[_]](implicit f: LocallyConcrete[F], g: Normed[G])
    extends ComposeLinear[F, G]
    with Normed[({type T[A] = (F[G[A]])})#T] {
    override def sum[A](v: F[G[A]])(implicit a: Additive[A]): A =
      f.sum(f.mapLC(v)(g.sum(_))(g.additive[A], a))
    override def taxicab[A](v: F[G[A]])(implicit a: Analytic[A]): A =
      f.sum(f.mapLC(v)(g.taxicab(_))(g.additive[A], a))
    override def dot[A](u: F[G[A]], v: F[G[A]])(implicit field: Field[A]): A =
      f.sum(f.apLC(v)(f.mapLC(u)((x: G[A]) => g.dot(x, _: G[A]))(g.additive[A], Function.additive))(g.additive[A], field))
  }

  class ComposeLocallyConcrete[F[_], G[_]](implicit f_ : LocallyConcrete[F], g_ : LocallyConcrete[G])
    extends ComposeNormed[F, G]
    with LocallyConcrete[({type T[A] = (F[G[A]])})#T] {
    val f = f_
    val g = g_

    /**
     * A countable or finite set indexing the basis.
     */
    override type Index = (f.Index, g.Index)

    /**
     * The index must be recursively enumerable.
     */
    override def enumerateIndex: Enumerate[Index] =
      Enumerate.product(f.enumerateIndex, g.enumerateIndex)
    
    /**
     * The (normal) basis for this vector space.
     */
    override def basis[A](i: Index)(implicit field: Field[A]): F[G[A]] = {
      val fb = f.basis(i._1)
      val gb = g.basis(i._2)
      f.mapLC(fb)(g.mull(_, gb))(field, g.additive)
    }

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulateLC[A](h: Map[Index, A])(implicit a: Additive[A]): F[G[A]] =
      f.tabulateLC(h.groupBy(_._1._1).mapValues(x => g.tabulateLC(x.map{ case (k, v) => (k._2, v) })))(g.additive)

    /**
     * Find the coefficient of a basis vector.
     */
    override def indexLC[A](v: F[G[A]])(i: Index)(implicit a: Additive[A]): A =
      g.indexLC(f.indexLC(v)(i._1)(g.additive))(i._2)

    /**
     * Maps the vector with a function f. It must hold that f(0) = 0.
     */
    override def mapLC[A, B](x: F[G[A]])(h: (A) => B)(implicit a: Additive[A], b: Additive[B]): F[G[B]] =
      f.mapLC(x)(g.mapLC(_)(h))(g.additive, g.additive)

    /**
     * Applies a vector of functions to a vector, pointwise. It must hold that f(0) = 0.
     */
    override def apLC[A, B](x: F[G[A]])(h: F[G[(A) => B]])(implicit a: Additive[A], b: Additive[B]): F[G[B]] =
      f.apLC(x)(
        f.mapLC(h)(
          (gab: G[A => B]) => g.apLC(_: G[A])(gab)
        )(g.additive[(A) => B](Function.additive), Function.additive[G[A], G[B]](g.additive)))(g.additive[A], g.additive[B])

    /**
     * Returns the concrete subspace containing v.
     */
    override def restrict[A](v: F[G[A]])(implicit field: Field[A]): Concrete[({type T[A] = (F[G[A]])})#T] = {
      val fv: F[A] = f.mapLC(v)(g.sum(_))(g.additive, field)
      val gv: G[A] = f.sum(v)(g.additive)
      Compose[F, G].concrete(f.restrict(fv), g.restrict(gv))
    }

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
    override def restrict[A](h: (F[G[A]]) => A)(v: F[G[A]])
        (implicit a: Additive[A]): Concrete[({type T[A] = F[G[A]]})#T] = {
      val fv: F[A] = f.mapLC(v)(g.sum(_))(g.additive, a)
      val gv: G[A] = f.sum(v)(g.additive)

      val x = f.restrict((u: F[A]) => h(f.mapLC[A, G[A]](u)(_ => g.zero)(a, g.additive)))(fv)
      val y = g.restrict((u: G[A]) => h(x.point(u)))(gv)

      Compose[F, G].concrete(x, y)
    }
  }

  class ComposeConcrete[F[_], G[_]](implicit f_ : Concrete[F], g_ : Concrete[G])
    extends ComposeLocallyConcrete[F, G]
    with Concrete[({type T[A] = (F[G[A]])})#T] {
    override val f = f_
    override val g = g_

    /**
     * The (finite) dimension of this vector space.
     */
    override val dimFin: BigInt = f.dimFin * g.dimFin

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulate[A](h: (Index) => A): F[G[A]] =
      f.tabulate(i => g.tabulate(j => h((i, j))))

    /**
     * Find the coefficient of a basis vector.
     */
    def index[A](v: F[G[A]])(i: Index): A =
      g.index(f.index(v)(i._1))(i._2)

    override def map[A, B](v: F[G[A]])(h: (A) => B): F[G[B]] =
      f.map(v)(g.map(_)(h))

    override def point[A](a: => A): F[G[A]] =
      f.point(g.point(a))

    override def ap[A, B](x: => F[G[A]])(h: => F[G[(A) => B]]): F[G[B]] =
      f.ap(x)(f.map(h)(gab => g.ap(_)(gab)))

    override def foldMap[A, B](v: F[G[A]])(h: (A) => B)(implicit F: Monoid[B]): B =
      f.foldMap(v)(g.foldMap(_)(h))

    override def foldRight[A, B](v: F[G[A]], z: => B)(h: (A, => B) => B): B =
      f.foldRight(v, z)(g.foldRight(_, _)(h))
  }
}
