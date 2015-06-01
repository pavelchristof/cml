package cml.algebra

trait SerializableFunction[-A, +B] extends Function[A, B] with Serializable

object SerializableFunction {
  case class Constant[-A, +B] (value: B) extends SerializableFunction[A, B] {
    def apply(x: A): B = value
  }

  case class Compose[-A, B, +C] (f: (B) => C, g: (A) => B) extends SerializableFunction[A, C] {
    def apply(x: A): C = f(g(x))
  }
}
