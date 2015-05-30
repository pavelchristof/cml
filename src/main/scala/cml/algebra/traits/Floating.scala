package cml.algebra.traits

/**
 * Floating point numbers.
 */
trait Floating[A] extends Real[A] {
  val posInfinity: A
  val negInfinity: A
  val epsilon: A
  val minValue: A
  val maxValue: A
  val nan: A

  def isNaN(a: A): Boolean
}

object Floating {
  implicit object FloatInst extends Floating[Float] {
    override def add(x: Float, y: Float): Float = x + y
    override def sub(x: Float, y: Float): Float = x - y
    override def neg(x: Float): Float = -x
    override val zero: Float = 0

    override def mul(x: Float, y: Float): Float = x * y
    override val one: Float = 1
    override def fromInt(n: Int): Float = n
    override def fromLong(n: Long): Float = n

    override def div(x: Float, y: Float): Float = x / y
    override def inv(x: Float): Float = 1 / x

    override def abs(x: Float): Float = math.abs(x)
    override def signum(x: Float): Float = math.signum(x)
    override def exp(x: Float): Float = math.exp(x).toFloat
    override def log(x: Float): Float = math.log(x).toFloat
    override def sqrt(x: Float): Float = math.sqrt(x).toFloat
    override def sin(x: Float): Float = math.sin(x).toFloat
    override def cos(x: Float): Float = math.cos(x).toFloat
    override def tan(x: Float): Float = math.tan(x).toFloat
    override def asin(x: Float): Float = math.asin(x).toFloat
    override def acos(x: Float): Float = math.acos(x).toFloat
    override def atan(x: Float): Float = math.atan(x).toFloat
    override def sinh(x: Float): Float = math.sinh(x).toFloat
    override def cosh(x: Float): Float = math.cosh(x).toFloat
    override def tanh(x: Float): Float = math.tanh(x).toFloat

    override def fromFloat(x: Float): Float = x
    override def fromDouble(x: Double): Float = x.toFloat

    override def toFloat(x: Float): Float = x
    override def toDouble(x: Float): Double = x.toDouble
    override def compare(x: Float, y: Float): Int = x.compare(y)

    override val posInfinity: Float = Float.PositiveInfinity
    override val negInfinity: Float = Float.NegativeInfinity
    override val epsilon: Float = Float.MinPositiveValue
    override val minValue: Float = Float.MinValue
    override val maxValue: Float = Float.MaxValue
    override val nan: Float = Float.NaN
    override def isNaN(x: Float): Boolean = x.isNaN
  }

  implicit object DoubleInst extends Floating[Double] {
    override def add(x: Double, y: Double): Double = x + y
    override def sub(x: Double, y: Double): Double = x - y
    override def neg(x: Double): Double = -x
    override val zero: Double = 0

    override def mul(x: Double, y: Double): Double = x * y
    override val one: Double = 1
    override def fromInt(n: Int): Double = n
    override def fromLong(n: Long): Double = n

    override def div(x: Double, y: Double): Double = x / y
    override def inv(x: Double): Double = 1 / x

    override def abs(x: Double): Double = math.abs(x)
    override def signum(x: Double): Double = math.signum(x)
    override def exp(x: Double): Double = math.exp(x).toFloat
    override def log(x: Double): Double = math.log(x).toFloat
    override def sqrt(x: Double): Double = math.sqrt(x).toFloat
    override def sin(x: Double): Double = math.sin(x).toFloat
    override def cos(x: Double): Double = math.cos(x).toFloat
    override def tan(x: Double): Double = math.tan(x).toFloat
    override def asin(x: Double): Double = math.asin(x).toFloat
    override def acos(x: Double): Double = math.acos(x).toFloat
    override def atan(x: Double): Double = math.atan(x).toFloat
    override def sinh(x: Double): Double = math.sinh(x).toFloat
    override def cosh(x: Double): Double = math.cosh(x).toFloat
    override def tanh(x: Double): Double = math.tanh(x).toFloat

    override def fromFloat(x: Float): Double = x
    override def fromDouble(x: Double): Double = x

    override def toFloat(x: Double): Float = x.toFloat
    override def toDouble(x: Double): Double = x
    override def compare(x: Double, y: Double): Int = x.compare(y)

    override val posInfinity: Double = Double.PositiveInfinity
    override val negInfinity: Double = Double.NegativeInfinity
    override val epsilon: Double = Double.MinPositiveValue
    override val minValue: Double = Double.MinValue
    override val maxValue: Double = Double.MaxValue
    override val nan: Double = Double.NaN
    override def isNaN(x: Double): Boolean = x.isNaN
  }
}
