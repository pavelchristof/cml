package cml.ad

import cml.algebra._

object Forward extends Engine {
  case class Aug[A] (
    _1: A,
    _2: A
  )

  type Context[A] = DummyImplicit

  override implicit def zero[A](implicit z: Zero[A]): Zero[Aug[A]] = new Zero[Aug[A]] {
    override val zero: Aug[A] = Aug(z.zero, z.zero)
  }

  private class AugField[A](implicit f: Field[A]) extends Field[Aug[A]] {
    import f.fieldSyntax._

    override val zero: Aug[A] =
      Aug(_0, _0)

    override def add(x: Aug[A], y: Aug[A]): Aug[A] =
      Aug(x._1 + y._1, x._2 + y._2)

    override def neg(x: Aug[A]): Aug[A] =
      Aug(-x._1, -x._2)

    override val one: Aug[A] =
      Aug(_1, _0)

    override def mul(x: Aug[A], y: Aug[A]): Aug[A] =
      Aug(x._1 * y._1, x._1 * y._2 + x._2 * y._1)

    override def inv(x: Aug[A]): Aug[A] =
      Aug(f.inv(x._1), -x._2 / (x._1 * x._1))

    override def fromInt(n: Int): Aug[A] =
      Aug(f.fromInt(n), _0)
  }

  private class AugAnalytic[A](implicit f: Analytic[A]) extends AugField[A] with Analytic[Aug[A]] {
    import f.analyticSyntax._

    override def abs(x: Aug[A]): Aug[A] =
      Aug(x._1.abs, x._2.signum)

    override def signum(x: Aug[A]): Aug[A] =
      Aug(x._1.signum, _0)

    override def exp(x: Aug[A]): Aug[A] = {
      val y = x._1.exp
      Aug(y, y * x._2)
    }

    override def log(x: Aug[A]): Aug[A] =
      Aug(x._1.log, x._2 / x._1)

    override def sqrt(x: Aug[A]): Aug[A] = {
      val y = x._1.sqrt
      Aug(y, x._2 / (y + y))
    }

    override def sin(x: Aug[A]): Aug[A] =
      Aug(x._1.sin, x._2 * x._1.cos)

    override def cos(x: Aug[A]): Aug[A] =
      Aug(x._1.cos, -x._2 * x._1.sin)

    override def tan(x: Aug[A]): Aug[A] =
      Aug(x._1.tan, (x._2 + x._2) / (_1 + (x._1 + x._1).cos))

    override def asin(x: Aug[A]): Aug[A] =
      Aug(x._1.asin, x._2 / (_1 + -x._1 * x._1).sqrt)

    override def acos(x: Aug[A]): Aug[A] =
      Aug(x._1.acos, -x._2 / (_1 + -x._1 * x._1).sqrt)

    override def atan(x: Aug[A]): Aug[A] =
      Aug(x._1.atan, x._2 / (_1 + x._1 * x._1))

    override def sinh(x: Aug[A]): Aug[A] =
      Aug(x._1.sinh, x._2 * x._1.cosh)

    override def cosh(x: Aug[A]): Aug[A] =
      Aug(x._1.cosh, x._2 * x._1.sinh)

    override def tanh(x: Aug[A]): Aug[A] = {
      val q = x._1.cosh
      val p = _1 + (x._1 + x._1).cosh
      Aug(x._1.tanh, (x._2 * q * q * f.fromInt(4)) / (p * p))
    }

    override def fromFloat(x: Float): Aug[A] =
      Aug(f.fromFloat(x), _0)

    override def fromDouble(x: Double): Aug[A] =
      Aug(f.fromDouble(x), _0)
  }

  /**
   * Aug[A] is field given that F is one.
   */
  override implicit def field[A](implicit f: Field[A], ctx: Context[A]): Field[Aug[A]] = new AugField[A]

  /**
   * Aug[A] is an analytic field given that F is one.
   */
  override implicit def analytic[A](implicit f: Analytic[A], ctx: Context[A]): Analytic[Aug[A]] = new AugAnalytic[A]

  /**
   * Injects a constant value into the augmented field.
   */
  override def constant[A](x: A)(implicit a: Zero[A]): Aug[A] =
    Aug(x, a.zero)

  /**
   * Differentiates a function.
   */
  override def diff[A](f: (Aug[A]) => (Context[A]) => Aug[A])(implicit field: Field[A]): (A) => A =
    diffWithValue(f)(field)(_)._2

  /**
   * Computes a function value and its derivative.
   */
  override def diffWithValue[A](f: (Aug[A]) => (Context[A]) => Aug[A])(implicit field: Field[A]): (A) => (A, A) =
    x => {
      val r = f(Aug(x, field.one))(DummyImplicit.dummyImplicit)
      (r._1, r._2)
    }

  /**
   * Computes the gradient of a function taking a vector as the argument.
   */
  override def grad[A, V[_]](f: (V[Aug[A]]) => (Context[A]) => Aug[A])
      (implicit field: Field[A], space: Cartesian[V]): (V[A]) => V[A] = x => {
    space.tabulate(i => {
      val input = space.tabulate(j =>
        Aug(space.index(x)(i), if (i == j) field.one else field.zero))
      f(input)(DummyImplicit.dummyImplicit)._2
    })
  }

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  override def gradWithValue[A, V[_]](f: (V[Aug[A]]) => (Context[A]) => Aug[A])
      (implicit field: Field[A], space: Cartesian[V]): (V[A]) => (A, V[A]) = x => {
    val value = f(space.tabulate(i => Aug(space.index(x)(i), field.zero)))(DummyImplicit.dummyImplicit)._1
    (value, grad(f)(field, space)(x))
  }
}
