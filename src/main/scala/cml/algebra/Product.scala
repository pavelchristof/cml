package cml.algebra

import cml.algebra.traits._
import shapeless.ops.nat.{ToInt, Sum}

import scalaz.Monoid

object Product {
  class ProductAdditive[F, G](implicit f: Additive[F], g: Additive[G])
    extends Additive[(F, G)] {
    override val zero = (f.zero, g.zero)
    override def add(x: (F, G), y: (F, G)): (F, G) = (f.add(x._1, y._1), g.add(x._2, y._2))
    override def neg(x: (F, G)): (F, G) = (f.neg(x._1), g.neg(x._2))
  }

  class ProductRing[F, G](implicit f: Ring[F], g: Ring[G])
    extends ProductAdditive[F, G] with Ring[(F, G)] {
    override val one: (F, G) = (f.one, g.one)
    override def mul(x: (F, G), y: (F, G)): (F, G) = (f.mul(x._1, y._1), g.mul(x._2, y._2))
  }

  class ProductField[F, G](implicit f: Field[F], g: Field[G])
    extends ProductRing[F, G] with Field[(F, G)] {
    override def inv(x: (F, G)): (F, G) = (f.inv(x._1), g.inv((x._2)))
  }

  class ProductAdditive1[F[_], G[_]](implicit f: Additive1[F], g: Additive1[G])
    extends Additive1[({type T[A] = (F[A], G[A])})#T] {
    override def zero[A](implicit a: Additive[A]): (F[A], G[A]) = (f.zero, g.zero)
    override def add[A](x: (F[A], G[A]), y: (F[A], G[A]))(implicit a: Additive[A]): (F[A], G[A]) =
      (f.add(x._1, y._1), g.add(x._2, y._2))
    override def neg[A](x: (F[A], G[A]))(implicit a: Additive[A]): (F[A], G[A]) =
      (f.neg(x._1), g.neg(x._2))
  }

  class ProductLinear[F[_], G[_]](implicit f: Linear[F], g: Linear[G])
    extends ProductAdditive1[F, G]
    with Linear[({type T[A] = (F[A], G[A])})#T] {
    override def mull[A](a: A, v: (F[A], G[A]))(implicit field: Field[A]): (F[A], G[A]) =
      (f.mull(a, v._1), g.mull(a, v._2))
    override def mulr[A](v: (F[A], G[A]), a: A)(implicit field: Field[A]): (F[A], G[A]) =
      (f.mulr(v._1, a), g.mulr(v._2, a))
    override def div[A](v: (F[A], G[A]), a: A)(implicit field: Field[A]): (F[A], G[A]) =
      (f.div(v._1, a), g.div(v._2, a))
  }

  class ProductNormed[F[_], G[_]](implicit f: Normed[F], g: Normed[G])
    extends ProductLinear[F, G]
    with Normed[({type T[A] = (F[A], G[A])})#T] {
    override def taxicab[A](v: (F[A], G[A]))(implicit a: Analytic[A]): A =
      a.add(f.taxicab(v._1), g.taxicab(v._2))
    override def dot[A](u: (F[A], G[A]), v: (F[A], G[A]))(implicit a: Field[A]): A =
      a.add(f.dot(u._1, v._1), g.dot(u._2, v._2))
  }

  class ProductConcrete[F[_], G[_]](implicit f: Concrete[F], g: Concrete[G])
    extends ProductNormed[F, G]
    with Concrete[({type T[A] = (F[A], G[A])})#T] {
    /**
     * Construct a vector using given coefficients for the orthonormal basis.
     */
    override def tabulate[A](h: (Int) => A): (F[A], G[A]) =
      (f.tabulate(h), g.tabulate(i => h(i + f.dim())))

    /**
     * Find the coefficient of the i-th basis vector.
     */
    override def index[A](v: (F[A], G[A]))(i: Int): A =
      if (i < f.dim()) {
        f.index(v._1)(i)
      } else {
        g.index(v._2)(i - f.dim())
      }

    /**
     * Returns the i-th vector of the orthonormal basis.
     * @param i Index assumed to be in range [0, dim - 1].
     */
    override def unit[A](i: Int)(implicit a: Ring[A]): (F[A], G[A]) =
      if (i < f.dim()) {
        (f.unit(i), g.zero)
      } else {
        (f.zero, g.unit(i - f.dim()))
      }

    /**
     * Applies a function pointwise on the coordinates of the vector.
     */
    override def pointwise[A](h: AnalyticMap)(v: (F[A], G[A]))(implicit a: Analytic[A]): (F[A], G[A]) =
      (f.pointwise(h)(v._1), g.pointwise(h)(v._2))

    // Private value would escape its scope blah blah blah
    val concrete1 = f
    val concrete2 = g

    /**
     * The dimension of this vector space, as a type.
     */
    override type Dim = Sum[concrete1.Dim, concrete2.Dim]#Out

    /**
     * The dimension of this vector space.
     */
    override val dim: ToInt[Dim] = new ToInt[Dim] {
      override def apply(): Int = f.dim() + g.dim()
    }

    /**
     * Applies a function, possibly changing the underlying field.
     */
    override def map[A, B](v: (F[A], G[A]))(h: (A) => B): (F[B], G[B]) =
      (f.map(v._1)(h), g.map(v._2)(h))

    /**
     * Sets all coefficients to some value.
     */
    override def point[A](a: => A): (F[A], G[A]) =
      (f.point(a), g.point(a))

    /**
     * Applies functions pointwise.
     */
    override def ap[A, B](x: => (F[A], G[A]))(h: => (F[(A) => B], G[(A) => B])): (F[B], G[B]) =
      (f.ap(x._1)(h._1), g.ap(x._2)(h._2))

    override def foldMap[A, B](v: (F[A], G[A]))(op: (A) => B)(implicit m: Monoid[B]): B =
      m.append(f.foldMap(v._1)(op), g.foldMap(v._2)(op))

    override def foldRight[A, B](v: (F[A], G[A]), z: => B)(op: (A, => B) => B): B =
      f.foldRight(v._1, g.foldRight(v._2, z)(op))(op)
  }
  
  implicit def additive[F, G](implicit f: Additive[F], g: Additive[G]): Additive[(F, G)] = new ProductAdditive[F, G]()
  implicit def ring[F, G](implicit f: Ring[F], g: Ring[G]): Ring[(F, G)] = new ProductRing[F, G]()
  implicit def field[F, G](implicit f: Field[F], g: Field[G]): Field[(F, G)] = new ProductField[F, G]()
  
  implicit def additive1[F[_], G[_]]
    (implicit f: Additive1[F], g: Additive1[G]): Additive1[({type T[A] = (F[A], G[A])})#T] =
    new ProductAdditive1[F, G]()
  implicit def linear[F[_], G[_]]
    (implicit f: Linear[F], g: Linear[G]): Linear[({type T[A] = (F[A], G[A])})#T] =
    new ProductLinear[F, G]()
  implicit def normed[F[_], G[_]]
    (implicit f: Normed[F], g: Normed[G]): Normed[({type T[A] = (F[A], G[A])})#T] =
    new ProductNormed[F, G]()
  implicit def concrete[F[_], G[_]]
    (implicit f: Concrete[F], g: Concrete[G]): Concrete[({type T[A] = (F[A], G[A])})#T] =
    new ProductConcrete[F, G]()
}
