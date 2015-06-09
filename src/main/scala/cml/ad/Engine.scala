package cml.ad

import cml.algebra._

/**
 * Automatic differentiation engine.
 */
trait Engine extends Serializable {
  type Aug[_]
  type Context[_]

  implicit def zero[A](implicit z: Zero[A]): Zero[Aug[A]]

  /**
   * Aug[A] is a field given that F is one.
   */
  implicit def field[A](implicit f: Field[A], ctx: Context[A]): Field[Aug[A]]

  /**
   * Aug[A] is an analytic field given that F is one.
   */
  implicit def analytic[A](implicit f: Analytic[A], ctx: Context[A]): Analytic[Aug[A]]

  /**
   * Injects a constant value into the augmented field.
   */
  def constant[A](x: A)(implicit a: Zero[A]): Aug[A]

  /**
   * Differentiates a function.
   */
  def diff[A](f: (Aug[A]) => (Context[A]) => Aug[A])(implicit field: Field[A]): (A) => A

  /**
   * Computes a function value and its derivative.
   */
  def diffWithValue[A](f: (Aug[A]) => (Context[A]) => Aug[A])(implicit field: Field[A]): (A) => (A, A)

  /**
   * Computes the gradient of a function taking a vector as the argument.
   */
  def grad[A, V[_]](f: (V[Aug[A]]) => (Context[A]) => Aug[A])
      (implicit field: Field[A], space: Cartesian[V]): (V[A]) => V[A]

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  def gradWithValue[A, V[_]](f: (V[Aug[A]]) => (Context[A]) => Aug[A])
      (implicit field: Field[A], space: Cartesian[V]): (V[A]) => (A, V[A])
}
