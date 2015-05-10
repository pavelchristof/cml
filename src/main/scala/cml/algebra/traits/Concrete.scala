package cml.algebra.traits

trait AnalyticMap {
  def apply[F](x: F)(implicit f: Analytic[F]): F
}

trait Concrete[V[_]] extends Normed[V] {
  def map[F](g: AnalyticMap)(v: V[F])(implicit f: Analytic[F]): V[F]
  def dim: Int
  
  def exp[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.exp(x)
  })(x)
  def log[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.log(x)
  })(x)
  def sqrt[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.sqrt(x)
  })(x)

  def sin[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.sin(x)
  })(x)
  def cos[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.cos(x)
  })(x)
  def tan[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.tan(x)
  })(x)

  def asin[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.asin(x)
  })(x)
  def acos[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.acos(x)
  })(x)
  def atan[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.atan(x)
  })(x)

  def sinh[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.sinh(x)
  })(x)
  def cosh[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.cosh(x)
  })(x)
  def tanh[F](x: V[F])(implicit f: Analytic[F]): V[F] = map(new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.tanh(x)
  })(x)
}
