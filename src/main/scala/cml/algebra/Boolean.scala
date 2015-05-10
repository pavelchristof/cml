package cml.algebra

import cml.algebra.traits.Field

object Boolean {
  implicit object BooleanField extends Field[Boolean] {
    override def add(x: Boolean, y: Boolean): Boolean = x ^ y
    override def neg(x: Boolean): Boolean = x
    override def mul(x: Boolean, y: Boolean): Boolean = x && y
    override def inv(x: Boolean): Boolean = true

    override val zero: Boolean = false
    override val one: Boolean = true
  }
}
