package cml.algebra.traits

trait Field[T] extends Ring[T] {
  def div(x: T, y: T): T = mul(x, inv(y))
  def inv(x: T): T
}
