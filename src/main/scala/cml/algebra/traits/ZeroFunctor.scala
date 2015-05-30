package cml.algebra.traits

/**
 * An endofunctor on the category of pointed types.
 */
trait ZeroFunctor[V[_]] {
  /**
   * Each object (A, zero : A) is mapped to an object (V[A], zero : V[A]).
   */
  def zero[A](implicit a: Zero[A]): V[A]

  def zeroInst[A](implicit a: Zero[A]): Zero[V[A]] = new Zero[V[A]] {
    override val zero: V[A] = ZeroFunctor.this.zero
  }

  /**
   * Lifts a zero preserving function.
   */
  def map[A, B](v: V[A])(f: (A) => B)(implicit a: Zero[A], b: Zero[B]): V[B]
}
