package cml.algebra.traits

import shapeless.Nat
import shapeless.ops.nat.ToInt

import scalaz.{Monoid, Foldable, Applicative}

/**
 * Finitely dimensional vector spaces with a canonical orthonormal basis.
 */
trait Concrete[V[_]] extends Normed[V] with Applicative[V] with Foldable[V] {
  /**
   * The dimension of this vector space, as a type.
   */
  type Dim <: Nat

  /**
   * The dimension of this vector space.
   */
  val dim: ToInt[Dim]

  /**
   * Construct a vector using given coefficients for the orthonormal basis.
   */
  def tabulate[F](f: (Int) => F): V[F]

  /**
   * Find the coefficient of the i-th basis vector.
   */
  def index[F](v: V[F])(i: Int): F

  /**
   * Returns the i-th vector of the orthonormal basis.
   * @param i Index assumed to be in range [0, dim - 1].
   */
  def unit[F](i: Int)(implicit f: Ring[F]): V[F] =
    tabulate(j => if (i == j) f.one else f.zero)

  /**
   * Applies a function, possibly changing the underlying field.
   */
  override def map[A, B](v: V[A])(f: (A) => B): V[B] = {
    val coeff = index(v)_
    tabulate(i => f(coeff(i)))
  }

  /**
   * Sets all coefficients to some value.
   */
  override def point[A](a: => A): V[A] = tabulate(_ => a)

  /**
   * Applies functions pointwise.
   */
  override def ap[A, B](x: => V[A])(f: => V[(A) => B]): V[B] = {
    val xi = index(x)_
    val fi = index(f)_
    tabulate(i => fi(i)(xi(i)))
  }

  /**
   * Applies a function pointwise on the coordinates of the vector.
   *
   * Restricted to natural maps.
   */
  def pointwise[F](g: AnalyticMap)(v: V[F])(implicit f: Analytic[F]): V[F] = {
    val coeff = index(v)_
    tabulate(i => g(coeff(i)))
  }

  /**
   * Sums all the coordinates.
   */
  def sum[F](v: V[F])(implicit f: Additive[F]): F = foldRight(v, f.zero)(f.add(_, _))

  override def foldMap[A, B](fa: V[A])(f: (A) => B)(implicit F: Monoid[B]): B =
    (0 until dim()).map(i => f(index(fa)(i))).fold(F.zero)(F.append(_, _))

  override def foldRight[A, B](fa: V[A], z: => B)(f: (A, => B) => B): B =
    (0 until dim()).map(i => index(fa)(i)).foldRight(z)(f(_, _))

  def abs[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.abs)(x)
  def signum[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.signum)(x)

  def exp[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.exp)(x)
  def log[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.log)(x)
  def sqrt[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.sqrt)(x)

  def sin[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.sin)(x)
  def cos[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.cos)(x)
  def tan[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.tan)(x)

  def asin[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.asin)(x)
  def acos[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.acos)(x)
  def atan[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.atan)(x)

  def sinh[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.sinh)(x)
  def cosh[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.cosh)(x)
  def tanh[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.tanh)(x)

  def sigmoid[F](x: V[F])(implicit f: Analytic[F]): V[F] = pointwise(AnalyticMap.sigmoid)(x)
}
