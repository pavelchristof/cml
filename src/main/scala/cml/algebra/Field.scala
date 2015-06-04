package cml.algebra

import cml.syntax.FieldSyntax

/**
 * A field.
 */
trait Field[T] extends AbelianRing[T] {
  def div(x: T, y: T): T = mul(x, inv(y))
  def inv(x: T): T

  /**
   * Adds one to itself n times.
   *
   * Default implementation performs O(log n) multiplications.
   */
  def fromInt(n: Int): T = {
    import fieldSyntax._
    n match {
      case 0 => zero
      case 1 => one
      case _ if n < 0 => -this.fromInt(-n)
      case _ if n % 2 == 0 => {
        val half = this.fromInt(n/2)
        half + half
      }
      case _ => {
        val nearlyHalf = this.fromInt(n/2)
        nearlyHalf + nearlyHalf + one
      }
    }
  }

  /**
   * Adds one to itself n times.
   *
   * Default implementation performs O(log n) multiplications.
   */
  def fromLong(n: Long): T = {
    import fieldSyntax._
    n match {
      case 0 => zero
      case 1 => one
      case _ if n < 0 => -this.fromLong(-n)
      case _ if n % 2 == 0 => {
        val half = this.fromLong(n/2)
        half + half
      }
      case _ => {
        val nearlyHalf = this.fromLong(n/2)
        nearlyHalf + nearlyHalf + one
      }
    }
  }

  @transient lazy val fieldSyntax = new FieldSyntax[T] { def F = Field.this }
}

object Field {
  implicit object Z2 extends Field[Boolean] {
    override val zero: Boolean = false
    override val one: Boolean = true
    override def add(x: Boolean, y: Boolean): Boolean = x ^ y
    override def neg(x: Boolean): Boolean = x
    override def mul(x: Boolean, y: Boolean): Boolean = x && y
    override def inv(x: Boolean): Boolean = true
  }
}
