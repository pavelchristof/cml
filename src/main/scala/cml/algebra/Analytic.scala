package cml.algebra

import cml.syntax.AnalyticSyntax

/**
 * Real or complex numbers.
 */
trait Analytic[T] extends Field[T] {
  def abs(x: T): T
  def signum(x: T): T

  def exp(x: T): T
  def log(x: T): T
  def sqrt(x: T): T

  def sin(x: T): T
  def cos(x: T): T
  def tan(x: T): T

  def asin(x: T): T
  def acos(x: T): T
  def atan(x: T): T

  def sinh(x: T): T
  def cosh(x: T): T
  def tanh(x: T): T

  def fromFloat(x: Float): T
  def fromDouble(x: Double): T

  val analyticSyntax = new AnalyticSyntax[T] { def F = Analytic.this }
}
