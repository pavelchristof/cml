package cml.algebra

import cml.Enumerate
import cml.algebra.traits._

import scala.reflect.ClassTag
import scalaz.{Functor, Monoid}

case class Product[F[_], G[_]] (implicit f_ : Concrete[F], g_ : Concrete[G])
  extends Product.ProductConcrete[F, G] {
  type Type[A] = (F[A], G[A])
}

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

  class ProductFunctor[F[_], G[_]](implicit f: Functor[F], g: Functor[G])
    extends Functor[({type T[A] = (F[A], G[A])})#T] {
    override def map[A, B](v: (F[A], G[A]))(h: (A) => B): (F[B], G[B]) =
      (f.map(v._1)(h), g.map(v._2)(h))
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

  class ProductLocallyConcrete[F[_], G[_]](implicit val f : LocallyConcrete[F], val g : LocallyConcrete[G])
    extends ProductLinear[F, G]
    with LocallyConcrete[({type T[A] = (F[A], G[A])})#T] {
    /**
     * A countable or finite set indexing the basis.
     */
    override type Index = Either[f.Index, g.Index]

    /**
     * The index must be recursively enumerable.
     */
    override def enumerateIndex: Enumerate[Index] = Enumerate.sum(f.enumerateIndex, g.enumerateIndex)

    /**
     * The (normal) basis for this vector space.
     */
    override def basis[A](i: Index)(implicit field: Field[A]): (F[A], G[A]) =
      i match {
        case Left(j) => (f.basis(j), g.zero)
        case Right(j) => (f.zero, g.basis(j))
      }

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulateLC[A](h: Map[Index, A])(implicit a: Additive[A]): (F[A], G[A]) = {
      val lefts = for ((i, v) <- h; j <- i.left.toSeq) yield (j, v)
      val rights = for ((i, v) <- h; j <- i.right.toSeq) yield (j, v)
      (f.tabulateLC(lefts.toMap), g.tabulateLC(rights.toMap))
    }

    /**
     * Find the coefficient of a basis vector.
     */
    override def indexLC[A](v: (F[A], G[A]))(i: Index)(implicit a: Additive[A]): A =
      i match {
        case Left(j) => f.indexLC(v._1)(j)
        case Right(j) => g.indexLC(v._2)(j)
      }

    /**
     * Maps the vector with a function f. It must hold that f(0) = 0.
     */
    override def map[A, B](x: (F[A], G[A]))(h: (A) => B)(implicit a: Additive[A], b: Additive[B]): (F[B], G[B]) =
      (f.map(x._1)(h), g.map(x._2)(h))

    /**
     * Applies a vector of functions to a vector, pointwise. It must hold that f(0) = 0.
     */
    override def ap[A, B](x: (F[A], G[A]))(h: (F[A => B], G[A => B]))(implicit a: Additive[A], b: Additive[B]): (F[B], G[B]) =
      (f.ap(x._1)(h._1), g.ap(x._2)(h._2))

    /**
     * Applies a binary function pointwise. If must hold that f(0, 0) = 0.
     */
    override def apply2[A, B, C](x: (F[A], G[A]), y: (F[B], G[B]))(h: (A, B) => C)
        (implicit a: Additive[A], b: Additive[B], c: Additive[C]): (F[C], G[C]) =
      (f.apply2(x._1, y._1)(h), g.apply2(x._2, y._2)(h))

    override def restrict(keys: Set[Index]): Subspace[({type T[A] = (F[A], G[A])})#T] = {
      val fKeys = keys.flatMap(_.left.toSeq)
      val gKeys = keys.flatMap(_.right.toSeq)
      ProductSubspace(f.restrict(fKeys), g.restrict(gKeys))
    }
  }

  class ProductNormed[F[_], G[_]](implicit f: Normed[F], g: Normed[G])
    extends ProductLocallyConcrete[F, G]
    with Normed[({type T[A] = (F[A], G[A])})#T] {
    override def sum[A](v: (F[A], G[A]))(implicit a: Additive[A]): A =
      a.add(f.sum(v._1), g.sum(v._2))
  }

  case class ProductSubspace[F[_], G[_]](
    x : Subspace[F], y : Subspace[G]
  )(implicit f : LocallyConcrete[F], g : LocallyConcrete[G])
    extends Subspace[({type T[A] = (F[A], G[A])})#T] {
    override type Type[A] = (x.Type[A], y.Type[A])

    override def inject[A](u: Type[A])(implicit a: Additive[A]): (F[A], G[A]) =
      (x.inject(u._1), y.inject(u._2))
    override def project[A](v: (F[A], G[A]))(implicit a: Additive[A]): Type[A] =
      (x.project(v._1), y.project(v._2))

    override implicit val concrete: Concrete[Type] =
      Product.concrete[x.Type, y.Type](x.concrete, y.concrete)
  }

  class ProductConcrete[F[_], G[_]](implicit override val f : Concrete[F], override val g : Concrete[G])
    extends ProductNormed[F, G]
    with Concrete[({type T[A] = (F[A], G[A])})#T] {
    /**
     * The (finite) dimension of this vector space.
     */
    override val dimFin: BigInt = f.dimFin + g.dimFin

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulate[A](h: (Index) => A)(implicit additive: Additive[A]): (F[A], G[A]) =
      (f.tabulate(i => h(Left(i))), g.tabulate(i => h(Right(i))))

    /**
     * Find the coefficient of a basis vector.
     */
    override def index[A](v: (F[A], G[A]))(i: Index): A =
      i match {
        case Left(j) => f.index(v._1)(j)
        case Right(j) => g.index(v._2)(j)
      }

    override def point[A](a: => A)(implicit additive: Additive[A]): (F[A], G[A]) =
      (f.point(a), g.point(a))
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
  implicit def locallyConcrete[F[_], G[_]]
    (implicit f: LocallyConcrete[F], g: LocallyConcrete[G]): LocallyConcrete[({type T[A] = (F[A], G[A])})#T] =
    new ProductLocallyConcrete[F, G]()
  implicit def concrete[F[_], G[_]]
    (implicit f: Concrete[F], g: Concrete[G]): Concrete[({type T[A] = (F[A], G[A])})#T] =
    new ProductConcrete[F, G]()
}
