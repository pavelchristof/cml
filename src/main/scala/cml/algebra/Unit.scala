package cml.algebra

import cml.algebra.traits._

object Unit {
  /**
   * This isn't really a field because zero == one, but it is useful.
   */
  implicit object UnitInst extends Analytic[Unit] {
    override val zero: Unit = ()

    override val one: Unit = ()

    override def neg(x: Unit): Unit = ()

    override def add(x: Unit, y: Unit): Unit = ()

    override def mul(x: Unit, y: Unit): Unit = ()

    override def abs(x: Unit): Unit = ()

    override def atan(x: Unit): Unit = ()

    override def acos(x: Unit): Unit = ()

    override def fromFloat(x: Float): Unit = ()

    override def fromDouble(x: Double): Unit = ()

    override def tanh(x: Unit): Unit = ()

    override def log(x: Unit): Unit = ()

    override def cosh(x: Unit): Unit = ()

    override def tan(x: Unit): Unit = ()

    override def cos(x: Unit): Unit = ()

    override def exp(x: Unit): Unit = ()

    override def asin(x: Unit): Unit = ()

    override def sqrt(x: Unit): Unit = ()

    override def sin(x: Unit): Unit = ()

    override def signum(x: Unit): Unit = ()

    override def sinh(x: Unit): Unit = ()

    override def inv(x: Unit): Unit = ()
  }
}
