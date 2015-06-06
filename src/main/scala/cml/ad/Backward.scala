package cml.ad

import cml.algebra._
import scala.collection.mutable

object Backward extends Engine {
  type Aug[A] = (Int, A)

  type Context[A] = TapeBuilder[A]

  class Tape[A] (
    val values: Array[A],
    val indices: Array[Int],
    val size: Int
  )

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

  private def unary[A](i: Int, d: A)(implicit tape: TapeBuilder[A]): Int =
    if (i >= 0) tape.newUnary(i, d) else -1

  private def binary[A](i1: Int, i2: Int, d1: A, d2: A)
      (implicit tape: TapeBuilder[A]): Int =
    if (i1 >= 0 && i2 >= 0) tape.newBinary(i1, i2, d1, d2)
    else if (i1 >= 0) tape.newUnary(i1, d1)
    else if (i2 >= 0) tape.newUnary(i2, d2)
    else -1

  private def backpropagate[@specialized(Float, Double) A](out: Int, b: TapeBuilder[A])
      (implicit a: AbelianRing[A]): Array[A] = {
    val tape = b.result()
    val arr = Array.fill[A](tape.size)(a.zero)
    arr(out) = a.one

    var i = tape.size - 1
    while (i >= 0) {
      val d = arr(i)

      val j1 = tape.indices(2*i)
      val j2 = tape.indices(2*i + 1)
      val d1 = tape.values(2*i)
      val d2 = tape.values(2*i + 1)

      if (j1 != -1)
        arr(j1) = a.add(arr(j1), a.mul(d1, d))

      if (j2 != -1)
        arr(j2) = a.add(arr(j2), a.mul(d2, d))

      i -= 1
    }

    arr
  }

  override implicit def zero[A](implicit z: Zero[A]): Zero[Aug[A]] = new Zero[Aug[A]] {
    override val zero: Aug[A] = (-1, z.zero)
  }

  private class AugField[A] (
    implicit f: Field[A],
    ctx: Context[A]
  ) extends Field[Aug[A]] {
    override val zero: Aug[A] = (-1, f.zero)
    override val one: Aug[A] = (-1, f.one)

    override def add(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, f.one, f.one), f.add(x._2, y._2))
    override def neg(x: Aug[A]): Aug[A] =
      (unary(x._1, f.neg(f.one)), f.neg(x._2))
    override def sub(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, f.one, f.neg(f.one)), f.sub(x._2, y._2))

    override def mul(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, y._2, x._2), f.mul(x._2, y._2))
    override def inv(x: Aug[A]): Aug[A] =
      (unary(x._1, f.neg(f.inv(f.square(x._2)))), f.inv(x._2))
    override def div(x: Aug[A], y: Aug[A]): Aug[A] =
      (binary(x._1, y._1, f.inv(y._2), f.neg(f.div(x._2, f.square(y._2)))), f.div(x._2, y._2))

    override def fromInt(n: Int): Aug[A] = (-1, f.fromInt(n))
  }

  private class AugAnalytic[A] (
    implicit a: Analytic[A],
    ctx: Context[A]
  ) extends AugField[A] with Analytic[Aug[A]] {
    override def abs(x: Aug[A]): Aug[A] =
      (unary(x._1, a.signum(x._2)), a.abs(x._2))
    override def signum(x: Aug[A]): Aug[A] =
      (unary(x._1, a.zero), a.signum(x._2))

    override def exp(x: Aug[A]): Aug[A] =
      (unary(x._1, a.exp(x._2)), a.exp(x._2))
    override def log(x: Aug[A]): Aug[A] =
      (unary(x._1, a.inv(x._2)), a.log(x._2))

    override def sqrt(x: Aug[A]): Aug[A] =
      (unary(x._1, a.inv(a.mul(a.fromInt(2), a.sqrt(x._2)))), a.sqrt(x._2))

    override def sin(x: Aug[A]): Aug[A] =
      (unary(x._1, a.cos(x._2)), a.sin(x._2))
    override def cos(x: Aug[A]): Aug[A] =
      (unary(x._1, a.neg(a.sin(x._2))), a.cos(x._2))
    override def tan(x: Aug[A]): Aug[A] =
      (unary(x._1, a.div(a.fromInt(2), a.add(a.one, a.cos(a.double(x._2))))), a.tan(x._2))

    override def asin(x: Aug[A]): Aug[A] =
      (unary(x._1, a.inv(a.sqrt(a.sub(a.one, a.square(x._2))))), a.asin(x._2))
    override def acos(x: Aug[A]): Aug[A] =
      (unary(x._1, a.neg(a.inv(a.sqrt(a.sub(a.one, a.square(x._2)))))), a.acos(x._2))
    override def atan(x: Aug[A]): Aug[A] =
      (unary(x._1, a.inv(a.add(a.one, a.square(x._2)))), a.atan(x._2))

    override def sinh(x: Aug[A]): Aug[A] =
      (unary(x._1, a.cosh(x._2)), a.sinh(x._2))
    override def cosh(x: Aug[A]): Aug[A] =
      (unary(x._1, a.sinh(x._2)), a.cosh(x._2))
    override def tanh(x: Aug[A]): Aug[A] =
      (unary(x._1, a.div(a.mul(a.fromInt(4), a.cosh(x._2)), a.square(a.add(a.cosh(a.double(x._2)), a.one)))), a.tanh(x._2))

    override def fromFloat(x: Float): Aug[A] = (-1, a.fromFloat(x))
    override def fromDouble(x: Double): Aug[A] = (-1, a.fromDouble(x))
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
  override def constant[A](x: A)(implicit field: Field[A]): Aug[A] = (-1, x)

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
    val res = f((0, x), ctx)
    if (res._1 >= 0) {
      val arr = backpropagate(res._1, ctx)
      (res._2, arr(0))
    } else {
      (res._2, field.zero)
    }
  }

  private def makeInput[A, V[_]](v: V[A], tape: TapeBuilder[A])
      (implicit space: Cartesian[V], f: Field[A]): V[Aug[A]] = {
    space.tabulate[Aug[A]](key => {
      (space.keyToInt(key), space.index(v)(key))
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
    if (res._1 >= 0) {
      val arr = backpropagate(res._1, tape)
      (res._2, makeGrad(arr))
    } else {
      (res._2, space.zero)
    }
  }
}
