package cml.algebra.ad

import cml.algebra.traits._

object Forward extends Engine {
  type Aug[F] = (F, F)

  private class AugField[F](implicit f: Field[F]) extends Field[Aug[F]] {
    import f.fieldSyntax._

    override val zero: Aug[F] =
      (f.zero, f.zero)

    override def add(x: Aug[F], y: Aug[F]): Aug[F] =
      (x._1 + y._1, x._1 + y._1)

    override def neg(x: Aug[F]): Aug[F] =
      (-x._1, -x._2)

    override val one: Aug[F] =
      (f.one, f.zero)

    override def mul(x: Aug[F], y: Aug[F]): Aug[F] =
      (x._1 * y._1, x._1 * y._2 + x._2 * y._1)

    override def inv(x: Aug[F]): Aug[F] =
      (f.inv(x._1), -x._2 / (x._1 * x._1))
  }

  private class AugAnalytic[F](implicit f: Analytic[F]) extends AugField[F] with Analytic[Aug[F]] {
    import f.analyticSyntax._

    override def abs(x: Aug[F]): Aug[F] =
      (x._1.abs, x._2.signum)

    override def signum(x: Aug[F]): Aug[F] =
      (x._1.signum, f.zero)

    override def exp(x: Aug[F]): Aug[F] = {
      val y = x._1.exp
      (y, y * x._2)
    }

    override def log(x: Aug[F]): Aug[F] =
      (x._1.log, x._2 / x._1)

    override def sqrt(x: Aug[F]): Aug[F] = {
      val y = x._1.sqrt
      (y, x._2 / (y + y))
    }

    override def sin(x: Aug[F]): Aug[F] =
      (x._1.sin, x._2 * x._1.cos)

    override def cos(x: Aug[F]): Aug[F] =
      (x._1.cos, -x._2 * x._1.sin)

    override def tan(x: Aug[F]): Aug[F] =
      (x._1.tan, (x._2 + x._2) / ((x._1 + x._1).cos + f.one))

    override def asin(x: Aug[F]): Aug[F] =
      (x._1.asin, x._2 / (f.one - x._1 * x._1).sqrt)

    override def acos(x: Aug[F]): Aug[F] =
      (x._1.acos, -x._2 / (f.one - x._1 * x._1).sqrt)

    override def atan(x: Aug[F]): Aug[F] =
      (x._1.atan, x._2 / (x._1 * x._1 + f.one))

    override def sinh(x: Aug[F]): Aug[F] =
      (x._1.sinh, x._2 * x._1.cosh)

    override def cosh(x: Aug[F]): Aug[F] =
      (x._1.cosh, x._2 * x._1.sinh)

    override def tanh(x: Aug[F]): Aug[F] = {
      val four = f.one + f.one + f.one + f.one
      val q = x._1.cosh
      val p = (x._1 + x._1).cosh + f.one
      (x._1.tanh, (four * x._2 * q * q) / (p * p))
    }
  }

  /**
   * Aug[F] is field given that F is one.
   */
  implicit def field[F](implicit f: Field[F]): Field[Aug[F]] = new AugField[F]

  /**
   * Aug[F] is an analytic field given that F is one.
   */
  implicit def analytic[F](implicit f: Analytic[F]): Analytic[Aug[F]] = new AugAnalytic[F]

  /**
   * Injects a constant value into the augmented field.
   */
  override def inject[F](x: F)(implicit field: Field[F]): Aug[F] =
    (x, field.zero)

  /**
   * Differentiates a function.
   */
  override def diff[F](f: (Aug[F]) => Aug[F])(x: F)(implicit field: Field[F]): F =
    diffWithValue(f)(x)._2

  /**
   * Computes a function value and its derivative.
   */
  override def diffWithValue[F](f: (Aug[F]) => Aug[F])(x: F)(implicit field: Field[F]): (F, F) =
    f((x, field.one))

  /**
   * Computes the gradient of a function taking a vector as the argument.
   */
  override def grad[F, V[_]](f: (V[Aug[F]]) => Aug[F])(x: V[F])
      (implicit field: Field[F], concrete: Concrete[V]): V[F] =
    concrete.tabulate(i => {
      val input = concrete.tabulate(j =>
        (concrete.index(x)(i), if (i == j) field.one else field.zero))
      f(input)._2
    })

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  override def gradWithValue[F, V[_]](f: (V[Aug[F]]) => Aug[F])(x: V[F])
      (implicit field: Field[F], concrete: Concrete[V]): (F, V[F]) = {
    val value = f(concrete.tabulate(i => (concrete.index(x)(i), field.zero)))._1
    (value, grad(f)(x))
  }
}
