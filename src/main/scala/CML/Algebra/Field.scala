package CML.Algebra

trait Field[T] extends Ring[T] {
  def div(x: T, y: T): T
  def inv(x: T): T
}
