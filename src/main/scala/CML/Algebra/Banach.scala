package CML.Algebra

trait Banach[V[_]] extends Normed[V] {
  def exp[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def log[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def sqrt[F](x: V[F])(implicit f: Analytic[F]): V[F]

  def sin[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def cos[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def tan[F](x: V[F])(implicit f: Analytic[F]): V[F]

  def asin[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def acos[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def atan[F](x: V[F])(implicit f: Analytic[F]): V[F]

  def sinh[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def cosh[F](x: V[F])(implicit f: Analytic[F]): V[F]
  def tanh[F](x: V[F])(implicit f: Analytic[F]): V[F]
}
