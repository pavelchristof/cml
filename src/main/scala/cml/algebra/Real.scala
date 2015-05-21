package cml.algebra

import cml.algebra.traits._

object Real {
  implicit object DoubleInst extends Floating[Double] {
    override def add(x: Double, y: Double): Double = x + y
    override def sub(x: Double, y: Double): Double = x - y
    override def neg(x: Double): Double = -x
    override val zero: Double = 0

    override def mul(x: Double, y: Double): Double = x * y
    override val one: Double = 1
    override def fromInt(n: Int): Double = n

    override def div(x: Double, y: Double): Double = x / y
    override def inv(x: Double): Double = 1 / x

    override def abs(x: Double): Double = math.abs(x)
    override def signum(x: Double): Double = math.signum(x)
    override def exp(x: Double): Double = math.exp(x)
    override def log(x: Double): Double = math.log(x)
    override def sqrt(x: Double): Double = math.sqrt(x)
    override def sin(x: Double): Double = math.sin(x)
    override def cos(x: Double): Double = math.cos(x)
    override def tan(x: Double): Double = math.tan(x)
    override def asin(x: Double): Double = math.asin(x)
    override def acos(x: Double): Double = math.acos(x)
    override def atan(x: Double): Double = math.atan(x)
    override def sinh(x: Double): Double = math.sinh(x)
    override def cosh(x: Double): Double = math.cosh(x)
    override def tanh(x: Double): Double = math.tanh(x)

    override def fromFloat(x: Float): Double = x
    override def fromDouble(x: Double): Double = x

    override def isNaN(x: Double): Boolean = x.isNaN
    override def toDouble(x: Double): Double = x
    override def toFloat(x: Double): Float = x.toFloat
    override val infinity: Double = Double.PositiveInfinity
  }
}
