package CML.Algebra

trait Additive[T] {
  val zero: T
  def add(x: T, y: T): T = sub(x, neg(y))
  def sub(x: T, y: T): T = add(x, neg(y))
  def neg(x: T): T = sub(zero, x)
}
