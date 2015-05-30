package cml.ad

import cml.algebra.{Field, Analytic, Additive}
import cml.algebra.traits._
import scala.collection.mutable.Builder

object Backward extends Engine {
  case class Aug[F] (
    _1: Option[Int],
    _2: F
  )

  sealed abstract class Cell[F]

  case class Nullary[F] () extends Cell[F]

  case class Unary[F] (
    i: Int,
    d: F
  ) extends Cell[F]

  case class Binary[F] (
    i1: Int, i2: Int,
    d1: F, d2: F
  ) extends Cell[F]

  case class Context[F] (
    tape: Builder[Cell[F], Array[Cell[F]]],
    var size: Int
  )

  private def newContext[F]: Context[F] = {
    val tape = Array.newBuilder[Cell[F]]
    tape.sizeHint(128)
    tape += Nullary()
    Context(tape, 1)
  }

  private def newContextV[F, V[_]](implicit space: Concrete[V]): Context[F] = {
    val tape = Array.newBuilder[Cell[F]]
    val n = space.dimFin.toInt
    tape.sizeHint(4 * n)
    var i = 0
    val e = Nullary[F]()
    while (i < n) {
      tape += e
      i += 1
    }
    Context(tape, n)
  }

  def newCell[F](cell: Cell[F])(implicit ctx: Context[F]): Option[Int] = cell match {
    case Nullary() => None
    case _ => {
      val i = ctx.size
      ctx.size += 1
      ctx.tape += cell
      Some(i)
    }
  }

  def unary[F](i: Option[Int], d: F): Cell[F] = i match {
    case Some(n) => Unary(n, d)
    case _ => Nullary()
  }

  def binary[F](i1: Option[Int], i2: Option[Int], d1: F, d2: F): Cell[F] = (i1, i2) match {
    case (Some(n), Some(m)) => Binary(n, m, d1, d2)
    case (Some(n), _) => Unary(n, d1)
    case (_, Some(m)) => Unary(m, d2)
    case (_, _) => Nullary()
  }

  private class AugField[F] (
    implicit f: Field[F],
    ctx: Context[F]
  ) extends Field[Aug[F]] {
    import f.fieldSyntax._

    override val zero: Aug[F] = Aug(None, _0)
    override val one: Aug[F] = Aug(None, _1)

    override def add(x: Aug[F], y: Aug[F]): Aug[F] =
      Aug(newCell(binary(x._1, y._1, _1, _1)), x._2 + y._2)
    override def neg(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, -_1)), -x._2)
    override def sub(x: Aug[F], y: Aug[F]): Aug[F] =
      Aug(newCell(binary(x._1, y._1, _1, -_1)), x._2 - y._2)

    override def mul(x: Aug[F], y: Aug[F]): Aug[F] =
      Aug(newCell(binary(x._1, y._1, y._2, x._2)), x._2 * y._2)
    override def inv(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, -f.inv(x._2.square))), f.inv(x._2))
    override def div(x: Aug[F], y: Aug[F]): Aug[F] =
      Aug(newCell(binary(x._1, y._1, f.inv(y._2), -x._2 / y._2.square)), x._2 / y._2)

    override def fromInt(n: Int): Aug[F] = Aug(None, f.fromInt(n))
  }

  private class AugAnalytic[F] (
    implicit an: Analytic[F],
    ctx: Context[F]
  ) extends AugField[F] with Analytic[Aug[F]] {
    import an.analyticSyntax._

    override def abs(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, x._2.signum)), x._2.abs)
    override def signum(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, _0)), x._2.signum)

    override def exp(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, x._2.exp)), x._2.exp)
    override def log(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, x._2.inv)), x._2.log)

    override def sqrt(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, (_2 * x._2.sqrt).inv)), x._2.sqrt)

    override def sin(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, x._2.cos)), x._2.sin)
    override def cos(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, -x._2.sin)), x._2.cos)
    override def tan(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, _2 / (_1 + (x._2 + x._2).cos))), x._2.tan)

    override def asin(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, (_1 - x._2.square).sqrt.inv)), x._2.asin)
    override def acos(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, -(_1 - x._2.square).sqrt.inv)), x._2.acos)
    override def atan(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, (_1 + x._2.square).inv)), x._2.atan)

    override def sinh(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, x._2.cosh)), x._2.sinh)
    override def cosh(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, x._2.sinh)), x._2.cosh)
    override def tanh(x: Aug[F]): Aug[F] =
      Aug(newCell(unary(x._1, (_4 * x._2.cosh) / ((x._2 + x._2).cosh + _1).square)), x._2.tanh)

    override def fromFloat(x: Float): Aug[F] = Aug(None, an.fromFloat(x))
    override def fromDouble(x: Double): Aug[F] = Aug(None, an.fromDouble(x))
  }

  /**
   * Aug[F] is a field given that F is one.
   */
  override implicit def field[F](implicit f: Field[F], ctx: Context[F]): Field[Aug[F]] = new AugField[F]

  /**
   * Aug[F] is an analytic field given that F is one.
   */
  override implicit def analytic[F](implicit f: Analytic[F], ctx: Context[F]): Analytic[Aug[F]] = new AugAnalytic[F]

  /**
   * Injects a constant value into the augmented field.
   */
  override def constant[F](x: F)(implicit field: Field[F]): Aug[F] = Aug(None, x)

  private def backpropagate[F](out: Int, ctx: Context[F])(implicit f: Field[F]): Array[F] = {
    import f.fieldSyntax._
    val tape = ctx.tape.result()
    val arr = Array.fill[F](ctx.size)(_0)
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
  override def diff[F](f: (Aug[F], Context[F]) => Aug[F])(implicit field: Field[F]): (F) => F = (x: F) =>
    diffWithValue(f)(field)(x)._2

  /**
   * Computes a function value and its derivative.
   */
  override def diffWithValue[F](f: (Aug[F], Context[F]) => Aug[F])
      (implicit field: Field[F]): (F) => (F, F) = (x: F) => {
    val ctx = newContext[F]
    val res = f(Aug(Some(0), x), ctx)
    res._1 match {
      case Some(i) => {
        val arr = backpropagate(i, ctx)
        (res._2, arr(0))
      }
      case None => (res._2, field.zero)
    }
  }

  private def makeInput[F, V[_]](v: V[F], ctx: Context[F])(implicit space: Concrete[V], f: Field[F]): V[Aug[F]] = {
    var i = 0
    space.tabulate(index => {
      val r = Aug(Some(i), space.index(v)(index))
      i += 1
      r
    })(field[F](f, ctx))
  }

  private def makeGrad[F, V[_]](arr: Array[F])(implicit space: Concrete[V], a: Additive[F]): V[F] = {
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
  override def grad[F, V[_]](f: (V[Aug[F]], Context[F]) => Aug[F])
      (implicit field: Field[F], space: Concrete[V]): (V[F]) => V[F] = (v: V[F]) =>
    gradWithValue(f)(field, space)(v)._2

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  override def gradWithValue[F, V[_]](f: (V[Aug[F]], Context[F]) => Aug[F])
      (implicit field: Field[F], space: Concrete[V]): (V[F]) => (F, V[F]) = (v: V[F]) => {
    val ctx = newContextV[F, V]
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