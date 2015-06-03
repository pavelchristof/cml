package cml.ad

import cml.algebra._
import scala.collection.mutable

object Backward extends Engine {
  type Aug[A] = (Option[Int], A)

  type Context[A] = TapeBuilder[A]

  class Tape[A] (
    val values: Array[A],
    val indices: Array[Int],
    val size: Int
  ) {
    def apply(i: Int): (Int, A, Int, A) =
      (indices(2*i), values(2*i), indices(2*i+1), values(2*i+1))
  }

  class TapeBuilder[A] (implicit a: Zero[A]) {
    val values: mutable.Builder[A, Array[A]] = Array.newBuilder
    val indices: mutable.Builder[Int, Array[Int]] = Array.newBuilder
    var index: Int = 0

    def newNullary(): Int = {
      indices += -1
      indices += -1
      values += a.zero
      values += a.zero
      val r = index
      index += 1
      r
    }

    def newUnary(i: Int, d: A): Int = {
      indices += i
      indices += -1
      values += d
      values += a.zero
      val r = index
      index += 1
      r
    }

    def newBinary(i1: Int, i2: Int, d1: A, d2: A): Int = {
      indices += i1
      indices += i2
      values += d1
      values += d2
      val r = index
      index += 1
      r
    }

    def sizeHint(size: Int) = {
      values.sizeHint(size * 2)
      indices.sizeHint(size * 2)
    }

    def result(): Tape[A] =
      new Tape[A](values.result(), indices.result(), index)
  }

  private def tapeBuilder[A](implicit a: Zero[A]): TapeBuilder[A] = {
    val tape = new TapeBuilder[A]()
    tape.sizeHint(1024 * 64)
    tape.newNullary()
    tape
  }

  private def tapeBuilderVec[A, V[_]](implicit a: Zero[A], space: Cartesian[V]): TapeBuilder[A] = {
    val tape = new TapeBuilder[A]()
    tape.sizeHint(space.dim * 2)
    var i = 0
    while (i < space.dim) {
      tape.newNullary()
      i += 1
    }
    tape
  }

  private def unary[A](i: Option[Int], d: A)(implicit tape: TapeBuilder[A]): Option[Int] = i match {
    case Some(n) => Some(tape.newUnary(n, d))
    case _ => None
  }

  private def binary[A](i1: Option[Int], i2: Option[Int], d1: A, d2: A)
      (implicit tape: TapeBuilder[A]): Option[Int] = (i1, i2) match {
    case (Some(n), Some(m)) => Some(tape.newBinary(n, m, d1, d2))
    case (Some(n), _) => Some(tape.newUnary(n, d1))
    case (_, Some(m)) => Some(tape.newUnary(m, d2))
    case (_, _) => None
  }

  private def backpropagate[A](out: Int, b: TapeBuilder[A])(implicit a: AbelianRing[A]): Array[A] = {
    val tape = b.result()
    val arr = Array.fill[A](tape.size)(a.zero)
    arr(out) = a.one

    var i = tape.size - 1
    while (i >= 0) {
      tape(i) match {
        case (-1, _, _, _) => ()
        case (j, d, -1, _) => arr(j) = a.add(arr(j), a.mul(d, arr(i)))
        case (j1, d1, j2, d2) => {
          val d = arr(i)
          arr(j1) = a.add(arr(j1), a.mul(d1, d))
          arr(j2) = a.add(arr(j2), a.mul(d2, d))
        }
      }
      i -= 1
    }

    arr
  }

  override implicit def zero[A](implicit z: Zero[A]): Zero[Aug[A]] = new Zero[Aug[A]] {
    override val zero: Aug[A] = (None, z.zero)
  }

  private class AugField[A] (
    implicit f: Field[A],
    ctx: Context[A]
  ) extends Field[Aug[A]] {
    import f.fieldSyntax._

    override val zero: Aug[A] = (None, _0)
    override val one: Aug[A] = (None, _1)

    override def add(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, _1, _1), x._2 + y._2)
    override def neg(x: Aug[A]): Aug[A] =
      (unary(x._1, -_1), -x._2)
    override def sub(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, _1, -_1), x._2 - y._2)

    override def mul(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, y._2, x._2), x._2 * y._2)
    override def inv(x: Aug[A]): Aug[A] =
      (unary(x._1, -f.inv(x._2.square)), f.inv(x._2))
    override def div(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, f.inv(y._2), -x._2 / y._2.square), x._2 / y._2)

    override def fromInt(n: Int): Aug[A] = (None, f.fromInt(n))
  }

  private class AugAnalytic[A] (
    implicit an: Analytic[A],
    ctx: Context[A]
  ) extends AugField[A] with Analytic[Aug[A]] {
    import an.analyticSyntax._

    override def abs(x: Aug[A]): Aug[A] =
      (unary(x._1, x._2.signum), x._2.abs)
    override def signum(x: Aug[A]): Aug[A] =
      (unary(x._1, _0), x._2.signum)

    override def exp(x: Aug[A]): Aug[A] =
      (unary(x._1, x._2.exp), x._2.exp)
    override def log(x: Aug[A]): Aug[A] =
      (unary(x._1, x._2.inv), x._2.log)

    override def sqrt(x: Aug[A]): Aug[A] =
      (unary(x._1, (_2 * x._2.sqrt).inv), x._2.sqrt)

    override def sin(x: Aug[A]): Aug[A] =
      (unary(x._1, x._2.cos), x._2.sin)
    override def cos(x: Aug[A]): Aug[A] =
      (unary(x._1, -x._2.sin), x._2.cos)
    override def tan(x: Aug[A]): Aug[A] =
      (unary(x._1, _2 / (_1 + (x._2 + x._2).cos)), x._2.tan)

    override def asin(x: Aug[A]): Aug[A] =
      (unary(x._1, (_1 - x._2.square).sqrt.inv), x._2.asin)
    override def acos(x: Aug[A]): Aug[A] =
      (unary(x._1, -(_1 - x._2.square).sqrt.inv), x._2.acos)
    override def atan(x: Aug[A]): Aug[A] =
      (unary(x._1, (_1 + x._2.square).inv), x._2.atan)

    override def sinh(x: Aug[A]): Aug[A] =
      (unary(x._1, x._2.cosh), x._2.sinh)
    override def cosh(x: Aug[A]): Aug[A] =
      (unary(x._1, x._2.sinh), x._2.cosh)
    override def tanh(x: Aug[A]): Aug[A] =
      (unary(x._1, (_4 * x._2.cosh) / ((x._2 + x._2).cosh + _1).square), x._2.tanh)

    override def fromFloat(x: Float): Aug[A] = (None, an.fromFloat(x))
    override def fromDouble(x: Double): Aug[A] = (None, an.fromDouble(x))
  }

  /**
   * Aug[A] is a field given that F is one.
   */
  override implicit def field[A](implicit f: Field[A], ctx: Context[A]): Field[Aug[A]] = new AugField[A]

  /**
   * Aug[A] is an analytic field given that F is one.
   */
  override implicit def analytic[A](implicit f: Analytic[A], ctx: Context[A]): Analytic[Aug[A]] = new AugAnalytic[A]

  /**
   * Injects a constant value into the augmented field.
   */
  override def constant[A](x: A)(implicit field: Field[A]): Aug[A] = (None, x)

  /**
   * Differentiates a function.
   */
  override def diff[A](f: (Aug[A], Context[A]) => Aug[A])(implicit field: Field[A]): (A) => A = (x: A) =>
    diffWithValue(f)(field)(x)._2

  /**
   * Computes a function value and its derivative.
   */
  override def diffWithValue[A](f: (Aug[A], Context[A]) => Aug[A])
      (implicit field: Field[A]): (A) => (A, A) = (x: A) => {
    val ctx = tapeBuilder[A]
    val res = f((Some(0), x), ctx)
    res._1 match {
      case Some(i) => {
        val arr = backpropagate(i, ctx)
        (res._2, arr(0))
      }
      case None => (res._2, field.zero)
    }
  }

  private def makeInput[A, V[_]](v: V[A], tape: TapeBuilder[A])
      (implicit space: Cartesian[V], f: Field[A]): V[Aug[A]] = {
    space.tabulate[Aug[A]](key => {
      (Some(space.keyToInt(key)), space.index(v)(key))
    })(field[A](f, tape))
  }

  private def makeGrad[A, V[_]](arr: Array[A])(implicit space: Cartesian[V], a: Additive[A]): V[A] = {
    space.tabulate(key => {
      arr(space.keyToInt(key))
    })
  }

  /**
   * Computes the gradient of a function taking a vector as the argument.
   */
  override def grad[A, V[_]](f: (V[Aug[A]], Context[A]) => Aug[A])
      (implicit field: Field[A], space: Cartesian[V]): (V[A]) => V[A] = (v: V[A]) =>
    gradWithValue(f)(field, space)(v)._2

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  override def gradWithValue[A, V[_]](f: (V[Aug[A]], Context[A]) => Aug[A])
      (implicit field: Field[A], space: Cartesian[V]): (V[A]) => (A, V[A]) = (v: V[A]) => {
    val tape = tapeBuilderVec[A, V]
    val res = f(makeInput(v, tape), tape)
    res._1 match {
      case Some(j) => {
        val arr = backpropagate(j, tape)
        (res._2, makeGrad(arr))
      }
      case None => (res._2, space.zero)
    }
  }
}