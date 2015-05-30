package cml.algebra

/**
 * Real numbers (and their approximations).
 */
trait Real[A] extends Analytic[A] with Ordering[A] {
  def toFloat(x: A): Float
  def toDouble(x: A): Double
}
