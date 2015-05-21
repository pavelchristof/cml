package cml.algebra.traits

trait Floating[A] extends Analytic[A] {
  def isNaN(x: A): Boolean

  val infinity: A

  def toFloat(x: A): Float
  def toDouble(x: A): Double
}
