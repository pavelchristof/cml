package cml.ad

import cml.algebra.traits._

object Forward extends Engine {
  case class Aug[F] (
    _1: F,
    _2: F
  )
  type Context[F] = DummyImplicit

  private class AugField[F](implicit f: Field[F]) extends Field[Aug[F]] {
    import f.fieldSyntax._

    override val zero: Aug[F] =
      Aug(_0, _0)

    override def add(x: Aug[F], y: Aug[F]): Aug[F] =
      Aug(x._1 + y._1, x._2 + y._2)

    override def neg(x: Aug[F]): Aug[F] =
      Aug(-x._1, -x._2)

    override val one: Aug[F] =
      Aug(_1, _0)

    override def mul(x: Aug[F], y: Aug[F]): Aug[F] =
      Aug(x._1 * y._1, x._1 * y._2 + x._2 * y._1)

    override def inv(x: Aug[F]): Aug[F] =
      Aug(f.inv(x._1), -x._2 / (x._1 * x._1))

    override def fromInt(n: Int): Aug[F] =
      Aug(f.fromInt(n), _0)

    override def runtimeClass: Class[AugField[F]] = classOf[AugField[F]]
  }

  private class AugAnalytic[F](implicit f: Analytic[F]) extends AugField[F] with Analytic[Aug[F]] {
    import f.analyticSyntax._

    override def abs(x: Aug[F]): Aug[F] =
      Aug(x._1.abs, x._2.signum)

    override def signum(x: Aug[F]): Aug[F] =
      Aug(x._1.signum, _0)

    override def exp(x: Aug[F]): Aug[F] = {
      val y = x._1.exp
      Aug(y, y * x._2)
    }

    override def log(x: Aug[F]): Aug[F] =
      Aug(x._1.log, x._2 / x._1)

    override def sqrt(x: Aug[F]): Aug[F] = {
      val y = x._1.sqrt
      Aug(y, x._2 / (y + y))
    }

    override def sin(x: Aug[F]): Aug[F] =
      Aug(x._1.sin, x._2 * x._1.cos)

    override def cos(x: Aug[F]): Aug[F] =
      Aug(x._1.cos, -x._2 * x._1.sin)

    override def tan(x: Aug[F]): Aug[F] =
      Aug(x._1.tan, (x._2 + x._2) / (_1 + (x._1 + x._1).cos))

    override def asin(x: Aug[F]): Aug[F] =
      Aug(x._1.asin, x._2 / (_1 + -x._1 * x._1).sqrt)

    override def acos(x: Aug[F]): Aug[F] =
      Aug(x._1.acos, -x._2 / (_1 + -x._1 * x._1).sqrt)

    override def atan(x: Aug[F]): Aug[F] =
      Aug(x._1.atan, x._2 / (_1 + x._1 * x._1))

    override def sinh(x: Aug[F]): Aug[F] =
      Aug(x._1.sinh, x._2 * x._1.cosh)

    override def cosh(x: Aug[F]): Aug[F] =
      Aug(x._1.cosh, x._2 * x._1.sinh)

    override def tanh(x: Aug[F]): Aug[F] = {
      val q = x._1.cosh
      val p = _1 + (x._1 + x._1).cosh
      Aug(x._1.tanh, (x._2 * q * q * f.fromInt(4)) / (p * p))
    }

    override def fromFloat(x: Float): Aug[F] =
      Aug(f.fromFloat(x), _0)

    override def fromDouble(x: Double): Aug[F] =
      Aug(f.fromDouble(x), _0)
  }

  /**
   * Aug[F] is field given that F is one.
   */
  override implicit def field[F](implicit f: Field[F], ctx: Context[F]): Field[Aug[F]] = new AugField[F]

  /**
   * Aug[F] is an analytic field given that F is one.
   */
  override implicit def analytic[F](implicit f: Analytic[F], ctx: Context[F]): Analytic[Aug[F]] = new AugAnalytic[F]

  /**
   * Injects a constant value into the augmented field.
   */
  override def constant[F](x: F)(implicit field: Field[F]): Aug[F] =
    Aug(x, field.zero)

  /**
   * Differentiates a function.
   */
  override def diff[F](f: (Aug[F], Context[F]) => Aug[F])(implicit field: Field[F]): (F) => F =
    diffWithValue(f)(field)(_)._2

  /**
   * Computes a function value and its derivative.
   */
  override def diffWithValue[F](f: (Aug[F], Context[F]) => Aug[F])(implicit field: Field[F]): (F) => (F, F) =
    x => {
      val r = f(Aug(x, field.one), DummyImplicit.dummyImplicit)
      (r._1, r._2)
    }

  /**
   * Computes the gradient of a function taking a vector as the argument.
   */
  override def grad[F, V[_]](f: (V[Aug[F]], Context[F]) => Aug[F])
      (implicit field: Field[F], space: Concrete[V]): (V[F]) => V[F] = x => {
    space.tabulate(i => {
      val input = space.tabulate(j =>
        Aug(space.index(x)(i), if (i == j) field.one else field.zero))
      f(input, DummyImplicit.dummyImplicit)._2
    })
  }

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  override def gradWithValue[F, V[_]](f: (V[Aug[F]], Context[F]) => Aug[F])
      (implicit field: Field[F], space: Concrete[V]): (V[F]) => (F, V[F]) = x => {
    val value = f(space.tabulate(i => Aug(space.index(x)(i), field.zero)), DummyImplicit.dummyImplicit)._1
    (value, grad(f)(field, space)(x))
  }

  /**
   * Computes the gradient of a function taking a vector as the argument.
   */
  override def gradLC[F, V[_]](f: (V[Aug[F]], DummyImplicit) => Aug[F])
      (implicit an: Analytic[F], space: LocallyConcrete[V]): (V[F]) => V[F] =
    x => grad[F, V](f)(an, space.restrict((x: V[Aug[F]]) => f(x, DummyImplicit.dummyImplicit))
      (space.mapLC(x)(constant(_))))(x)

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  override def gradWithValueLC[F, V[_]](f: (V[Aug[F]], DummyImplicit) => Aug[F])
      (implicit an: Analytic[F], space: LocallyConcrete[V]): (V[F]) => (F, V[F]) =
    x => gradWithValue[F, V](f)(an, space.restrict((x: V[Aug[F]]) => f(x, DummyImplicit.dummyImplicit))
      (space.mapLC(x)(constant(_))))(x)
}
