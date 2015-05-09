package CML.Algebra

trait Analytic[T] extends Field[T] {
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
}
