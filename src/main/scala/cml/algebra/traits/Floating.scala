package cml.algebra.traits

trait Floating[A] extends Analytic[A] {
  def isNaN(x: A): Boolean

  def toFloat(x: A): Float
  def toDouble(x: A): Double
}
