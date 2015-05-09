package CML.Algebra

trait Ring[T] extends Additive[T] {
  def mul(x: T, y: T): T
  val one: T
}
