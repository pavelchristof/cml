package cml.algebra.traits

import cml.syntax.FieldSyntax

trait Field[T] extends Ring[T] {
  def div(x: T, y: T): T = mul(x, inv(y))
  def inv(x: T): T

  val fieldSyntax = new FieldSyntax[T] { def F = Field.this }
}
