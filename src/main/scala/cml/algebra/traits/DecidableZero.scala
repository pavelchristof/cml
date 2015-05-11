package cml.algebra.traits

trait DecidableZero[F] extends Additive[F] {
  def isZero(x: F): Boolean
}
