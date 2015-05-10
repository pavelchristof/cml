package cml.algebra

import cml.algebra.traits._
import shapeless.ops.nat.{Prod, ToInt}

import scalaz.Monoid

object Compose {
  class ComposeAdditive1[F[_], G[_]](implicit f: Concrete[F], g: Additive1[G])
    extends Additive1[({type T[A] = F[G[A]]})#T] {
    override def zero[A](implicit field: Additive[A]): F[G[A]] = f.zero[G[A]](g.additive[A])
    override def add[A](x: F[G[A]], y: F[G[A]])(implicit field: Additive[A]): F[G[A]] = f.add(x, y)(g.additive[A])
    override def neg[A](x: F[G[A]])(implicit field: Additive[A]): F[G[A]] = f.neg(x)(g.additive[A])
  }

  class ComposeLinear[F[_], G[_]](implicit f: Concrete[F], g: Linear[G])
    extends ComposeAdditive1[F, G]
    with Linear[({type T[A] = F[G[A]]})#T] {
    override def mull[A](a: A, v: F[G[A]])(implicit field: Field[A]): F[G[A]] = f.map(v)(g.mull(a, _))
    override def mulr[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] = f.map(v)(g.mulr(_, a))
    override def div[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] = f.map(v)(g.div(_, a))
  }

  class ComposeNormed[F[_], G[_]](implicit f: Concrete[F], g: Normed[G])
    extends ComposeLinear[F, G]
    with Normed[({type T[A] = F[G[A]]})#T] {
    override def taxicab[A](v: F[G[A]])(implicit field: Analytic[A]): A = f.sum(f.map(v)(g.taxicab(_)))
    override def dot[A](u: F[G[A]], v: F[G[A]])(implicit field: Field[A]): A = f.sum(f.apply2(u, v)(g.dot(_, _)))
  }

  class ComposeConcrete[F[_], G[_]](implicit f: Concrete[F], g: Concrete[G])
    extends ComposeNormed[F, G]
    with Concrete[({type T[A] = F[G[A]]})#T] {
    val concrete1 = f
    val concrete2 = g

    override type Dim = Prod[concrete1.Dim, concrete2.Dim]#Out

    override val dim: ToInt[Dim] = new ToInt[Dim] {
      override def apply(): Int = f.dim() * g.dim()
    }

    /**
     * Construct a vector using given coefficients for the orthonormal basis.
     */
    override def tabulate[A](h: (Int) => A): F[G[A]] =
      f.tabulate(i => g.tabulate(j => h(i*g.dim() + j)))

    /**
     * Find the coefficient of the i-th basis vector.
     */
    override def index[A](v: F[G[A]])(i: Int): A =
      g.index(f.index(v)(i / g.dim()))(i % g.dim())

    /**
     * Applies a function, possibly changing the underlying field.
     */
    override def map[A, B](v: F[G[A]])(h: (A) => B): F[G[B]] =
      f.map(v)(g.map(_)(h))

    /**
     * Sets all coefficients to some value.
     */
    override def point[A](a: => A): F[G[A]] =
      f.point(g.point(a))

    /**
     * Applies functions pointwise.
     */
    override def ap[A, B](x: => F[G[A]])(h: => F[G[A => B]]): F[G[B]] =
      f.ap(x)(f.map(h)(gab => g.ap(_)(gab)))

    /**
     * Applies a function pointwise on the coordinates of the vector.
     *
     * Restricted to natural maps.
     */
    override def pointwise[A](h: AnalyticMap)(v: F[G[A]])(implicit field: Analytic[A]): F[G[A]] =
      f.map(v)(g.pointwise(h)(_))

    override def foldMap[A, B](v: F[G[A]])(h: (A) => B)(implicit F: Monoid[B]): B =
      f.foldMap(v)(g.foldMap(_)(h))

    override def foldRight[A, B](v: F[G[A]], z: => B)(h: (A, => B) => B): B =
      f.foldRight(v, z)(g.foldRight(_, _)(h))
  }

  implicit def additive1[F[_], G[_]]
    (implicit f: Concrete[F], g: Additive1[G]): Additive1[({type T[A] = F[G[A]]})#T] =
    new ComposeAdditive1[F, G]()
  implicit def linear[F[_], G[_]]
    (implicit f: Concrete[F], g: Linear[G]): Linear[({type T[A] = F[G[A]]})#T] =
    new ComposeLinear[F, G]()
  implicit def normed[F[_], G[_]]
    (implicit f: Concrete[F], g: Normed[G]): Normed[({type T[A] = F[G[A]]})#T] =
    new ComposeNormed[F, G]()
  implicit def concrete[F[_], G[_]]
    (implicit f: Concrete[F], g: Concrete[G]): Concrete[({type T[A] = F[G[A]]})#T] =
    new ComposeConcrete[F, G]()
}
