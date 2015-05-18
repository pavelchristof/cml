package cml.algebra

import cml.Enumerate
import cml.algebra.traits.Concrete

/**
 * Constant functor (and a 0 dimensional vector space).
 */
case class Constant[C, A] (value: C)

object Constant {
  def concrete[C](value: C): Concrete[({type T[A] = Constant[C, A]})#T] =
    new Concrete[({type T[A] = Constant[C, A]})#T] {
    override type Index = Void
    override def enumerateIndex: Enumerate[Index] = Enumerate.void

    override val dimFin: BigInt = 0

    /**
     * Construct a vector from coefficients of the basis vectors.
     */
    override def tabulate[A](h: (Void) => A): Constant[C, A] = Constant(value)

    /**
     * Find the coefficient of a basis vector.
     */
    override def index[A](v: Constant[C, A])(i: Void): A =
      // Cannot happen because Void is uninhabitable.
      i.asInstanceOf[A]
  }
}
