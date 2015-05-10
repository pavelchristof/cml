package cml.algebra.traits

/**
 * Finitely dimensional vector spaces with a canonical orthonormal basis.
 */
trait Concrete[V[_]] extends Normed[V] {
  /**
   * Construct a vector using given coefficients for the orthonormal basis.
   */
  def tabulate[F](f: (Int) => F)(implicit r: Field[F]): V[F]

  /**
   * Find the coefficient of the i-th basis vector.
   */
  def index[F](v: V[F])(i: Int)(implicit r: Field[F]): F

  /**
   * Returns the i-th vector of the orthonormal basis.
   * @param i Index assumed to be in range [0, dim - 1].
   */
  def unit[F](i: Int)(implicit f: Field[F]): V[F] =
    tabulate(j => if (i == j) f.one else f.zero)

  /**
   * Applies a function pointwise on the coordinates of the vector.
   */
  def pointwise[F](g: AnalyticMap)(v: V[F])(implicit f: Analytic[F]): V[F] = {
    val coeff = index(v)_
    tabulate(i => g(coeff(i)))
  }

  /**
   * The dimension of this vector space.
   */
  def dim: Int

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
