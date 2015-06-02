package cml.ad

import cml.algebra._
import scala.collection.mutable.Builder

object Backward extends Engine {
  case class Aug[A] (
    _1: Option[Int],
    _2: A
  )

  sealed abstract class Cell[A]

  case class Nullary[A] () extends Cell[A]

  case class Unary[A] (
    i: Int,
    d: A
  ) extends Cell[A]

  case class Binary[A] (
    i1: Int, i2: Int,
    d1: A, d2: A
  ) extends Cell[A]

  case class Context[A] (
    tape: Builder[Cell[A], Array[Cell[A]]],
    var size: Int
  )

  private def newContext[A]: Context[A] = {
    val tape = Array.newBuilder[Cell[A]]
    tape.sizeHint(128)
    tape += Nullary()
    Context(tape, 1)
  }

  private def newContextV[A, V[_]](implicit space: Cartesian[V]): Context[A] = {
    val tape = Array.newBuilder[Cell[A]]
    val n = space.dim
    tape.sizeHint(4 * n)
    var i = 0
    val e = Nullary[A]()
    while (i < n) {
      tape += e
      i += 1
    }
    Context(tape, n)
  }

  def newCell[A](cell: Cell[A])(implicit ctx: Context[A]): Option[Int] = cell match {
    case Nullary() => None
    case _ => {
      val i = ctx.size
      ctx.size += 1
      ctx.tape += cell
      Some(i)
    }
  }

  def unary[A](i: Option[Int], d: A): Cell[A] = i match {
    case Some(n) => Unary(n, d)
    case _ => Nullary()
  }

  def binary[A](i1: Option[Int], i2: Option[Int], d1: A, d2: A): Cell[A] = (i1, i2) match {
    case (Some(n), Some(m)) => Binary(n, m, d1, d2)
    case (Some(n), _) => Unary(n, d1)
    case (_, Some(m)) => Unary(m, d2)
    case (_, _) => Nullary()
  }

  override implicit def zero[A](implicit z: Zero[A]): Zero[Aug[A]] = new Zero[Aug[A]] {
    override val zero: Aug[A] = Aug(None, z.zero)
  }

  private class AugField[A] (
    implicit f: Field[A],
    ctx: Context[A]
  ) extends Field[Aug[A]] {
    import f.fieldSyntax._

    override val zero: Aug[A] = Aug(None, _0)
    override val one: Aug[A] = Aug(None, _1)

    override def add(x: Aug[A], y: Aug[A]): Aug[A] =
      Aug(newCell(binary(x._1, y._1, _1, _1)), x._2 + y._2)
    override def neg(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, -_1)), -x._2)
    override def sub(x: Aug[A], y: Aug[A]): Aug[A] =
      Aug(newCell(binary(x._1, y._1, _1, -_1)), x._2 - y._2)

    override def mul(x: Aug[A], y: Aug[A]): Aug[A] =
      Aug(newCell(binary(x._1, y._1, y._2, x._2)), x._2 * y._2)
    override def inv(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, -f.inv(x._2.square))), f.inv(x._2))
    override def div(x: Aug[A], y: Aug[A]): Aug[A] =
      Aug(newCell(binary(x._1, y._1, f.inv(y._2), -x._2 / y._2.square)), x._2 / y._2)

    override def fromInt(n: Int): Aug[A] = Aug(None, f.fromInt(n))
  }

  private class AugAnalytic[A] (
    implicit an: Analytic[A],
    ctx: Context[A]
  ) extends AugField[A] with Analytic[Aug[A]] {
    import an.analyticSyntax._

    override def abs(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, x._2.signum)), x._2.abs)
    override def signum(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, _0)), x._2.signum)

    override def exp(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, x._2.exp)), x._2.exp)
    override def log(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, x._2.inv)), x._2.log)

    override def sqrt(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, (_2 * x._2.sqrt).inv)), x._2.sqrt)

    override def sin(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, x._2.cos)), x._2.sin)
    override def cos(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, -x._2.sin)), x._2.cos)
    override def tan(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, _2 / (_1 + (x._2 + x._2).cos))), x._2.tan)

    override def asin(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, (_1 - x._2.square).sqrt.inv)), x._2.asin)
    override def acos(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, -(_1 - x._2.square).sqrt.inv)), x._2.acos)
    override def atan(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, (_1 + x._2.square).inv)), x._2.atan)

    override def sinh(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, x._2.cosh)), x._2.sinh)
    override def cosh(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, x._2.sinh)), x._2.cosh)
    override def tanh(x: Aug[A]): Aug[A] =
      Aug(newCell(unary(x._1, (_4 * x._2.cosh) / ((x._2 + x._2).cosh + _1).square)), x._2.tanh)

    override def fromFloat(x: Float): Aug[A] = Aug(None, an.fromFloat(x))
    override def fromDouble(x: Double): Aug[A] = Aug(None, an.fromDouble(x))
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
  override def constant[A](x: A)(implicit field: Field[A]): Aug[A] = Aug(None, x)

  private def backpropagate[A](out: Int, ctx: Context[A])(implicit f: Field[A]): Array[A] = {
    import f.fieldSyntax._
    val tape = ctx.tape.result()
    val arr = Array.fill[A](ctx.size)(_0)
    arr(out) = _1
    for (i <- (ctx.size - 1).to(0, -1)) {
      tape(i) match {
        case Nullary() => ()
        case Unary(j, d) => arr(j) = arr(j) + d * arr(i)
        case Binary(j1, j2, d1, d2) => {
          val d = arr(i)
          arr(j1) += d1 * d
          arr(j2) += d2 * d
        }
      }
    }
    arr
  }

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
    val ctx = newContext[A]
    val res = f(Aug(Some(0), x), ctx)
    res._1 match {
      case Some(i) => {
        val arr = backpropagate(i, ctx)
        (res._2, arr(0))
      }
      case None => (res._2, field.zero)
    }
  }

  private def makeInput[A, V[_]](v: V[A], ctx: Context[A])(implicit space: Cartesian[V], f: Field[A]): V[Aug[A]] = {
    var i = 0
    space.tabulate(index => {
      val r = Aug(Some(i), space.index(v)(index))
      i += 1
      r
    })(field[A](f, ctx))
  }

  private def makeGrad[A, V[_]](arr: Array[A])(implicit space: Cartesian[V], a: Additive[A]): V[A] = {
    var i = 0
    space.tabulate(_ => {
      val r = arr(i)
      i += 1
      r
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
    val ctx = newContextV[A, V]
    val res = f(makeInput(v, ctx), ctx)
    res._1 match {
      case Some(j) => {
        val arr = backpropagate(j, ctx)
        (res._2, makeGrad(arr))
      }
      case None => (res._2, space.zero)
    }
  }
}