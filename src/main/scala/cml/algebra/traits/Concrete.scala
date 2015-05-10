package cml.algebra.traits

trait Concrete[V[_]] extends Normed[V] {
  def pointwise[F](g: AnalyticMap)(v: V[F])(implicit f: Analytic[F]): V[F]
  def dim: Int
  
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
}
