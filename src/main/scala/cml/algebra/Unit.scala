package cml.algebra

import cml.algebra.traits.Ring

object Unit {
  implicit object UnitRing extends Ring[Unit] {
    override val zero: Unit = ()
    override val one: Unit = ()
    override def neg(x: Unit): Unit = ()
    override def add(x: Unit, y: Unit): Unit = ()
    override def mul(x: Unit, y: Unit): Unit = ()
  }
}
