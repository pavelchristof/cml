package cml.algebra.traits

import cml.syntax.RingSyntax

trait Ring[T] extends Additive[T] {
  def mul(x: T, y: T): T
  val one: T

  val ringSyntax = new RingSyntax[T] { def F = Ring.this }
}
