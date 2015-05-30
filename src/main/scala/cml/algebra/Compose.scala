package cml.algebra

import cml.Enumerate
import cml.algebra.Compose._
import cml.algebra.traits._

import scala.reflect.ClassTag
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
      f.map(v)(g.mull(a, _))(g.additive[A], g.additive[A])
    override def mulr[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] =
      f.map(v)(g.mulr(_, a))(g.additive[A], g.additive[A])
    override def div[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] =
      f.map(v)(g.div(_, a))(g.additive[A], g.additive[A])
  }

  class ComposeLocallyConcrete[F[_], G[_]](implicit f_ : LocallyConcrete[F], g_ : LocallyConcrete[G])
    extends ComposeLinear[F, G]
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
      f.map(fb)(g.mull(_, gb))(field, g.additive)
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
    override def map[A, B](x: F[G[A]])(h: (A) => B)(implicit a: Additive[A], b: Additive[B]): F[G[B]] =
      f.map(x)(g.map(_)(h))(g.additive, g.additive)

    /**
     * Applies a vector of functions to a vector, pointwise. It must hold that f(0) = 0.
     */
    override def ap[A, B](x: F[G[A]])(h: F[G[(A) => B]])(implicit a: Additive[A], b: Additive[B]): F[G[B]] =
      f.ap(x)(
        f.map(h)(
          (gab: G[A => B]) => g.ap(_: G[A])(gab)
        )(g.additive[(A) => B](Instances.functionAdditive), Instances.functionAdditive[G[A], G[B]](g.additive)))(g.additive[A], g.additive[B])

    /**
     * Applies a binary function pointwise. If must hold that f(0, 0) = 0.
     */
    override def apply2[A, B, C](x: F[G[A]], y: F[G[B]])(h: (A, B) => C)
        (implicit a: Additive[A], b: Additive[B], c: Additive[C]): F[G[C]] =
      f.apply2[G[A], G[B], G[C]](x, y)(
        g.apply2(_, _)(h)
      )(g.additive, g.additive, g.additive)

    override def restrict(keys: Set[Index]): Subspace[({type T[A] = F[G[A]]})#T] =
      ComposeSubspace[F, G](f.restrict(keys.map(_._1)), g.restrict(keys.map(_._2)))
  }

  class ComposeNormed[F[_], G[_]](implicit f: Normed[F], g: Normed[G])
    extends ComposeLocallyConcrete[F, G]
    with Normed[({type T[A] = (F[G[A]])})#T] {
    override def sum[A](v: F[G[A]])(implicit a: Additive[A]): A =
      f.sum(f.map(v)(g.sum(_))(g.additive[A], a))
  }

  case class ComposeSubspace[F[_], G[_]](x : Subspace[F], y : Subspace[G])(implicit f : LocallyConcrete[F], g : LocallyConcrete[G])
    extends Subspace[({type T[A] = (F[G[A]])})#T] {
    override type Type[A] = x.Type[y.Type[A]]

    override def inject[A](u: Type[A])(implicit a: Additive[A]): F[G[A]] =
      x.inject(x.concrete.map[y.Type[A], G[A]](u)(y.inject(_))(y.concrete.additive, g.additive))(g.additive)
    override def project[A](v: F[G[A]])(implicit a: Additive[A]): Type[A] =
      x.concrete.map(x.project(v)(g.additive))(y.project(_))(g.additive, y.concrete.additive)

    override implicit val concrete: Concrete[Type] =
      new ComposeConcrete[x.Type, y.Type]()(x.concrete, y.concrete)
  }

  case class ComposeConcrete[F[_], G[_]](implicit override val f: Concrete[F], override val g: Concrete[G])
    extends ComposeNormed[F, G]
    with Concrete[({type T[A] = (F[G[A]])})#T] {
    /**
     * The (finite) dimension of this vector space.
     */
    override val dimFin: BigInt = f.dimFin * g.dimFin

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulate[A](h: (Index) => A)(implicit additive: Additive[A]): F[G[A]] =
      f.tabulate(i => g.tabulate(j => h((i, j))))(g.additive[A])

    /**
     * Find the coefficient of a basis vector.
     */
    def index[A](v: F[G[A]])(i: Index): A =
      g.index(f.index(v)(i._1))(i._2)

    override def point[A](a: => A)(implicit additive: Additive[A]): F[G[A]] =
      f.point(g.point(a))(g.additive)
  }
}
