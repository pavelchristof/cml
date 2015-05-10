package cml.algebra.ad

import cml.algebra.traits._

/**
 * Automatic differentiation engine.
 */
trait Engine {
  type Aug[_]

  /**
   * Aug[F] is a field given that F is one.
   */
  implicit def field[F](implicit f: Field[F]): Field[Aug[F]]

  /**
   * Aug[F] is an analytic field given that F is one.
   */
  implicit def analytic[F](implicit f: Analytic[F]): Analytic[Aug[F]]

  /**
   * Injects a constant value into the augmented field.
   */
  def inject[F](x: F)(implicit field: Field[F]): Aug[F]

  /**
   * Differentiates a function.
   */
  def diff[F](f: (Aug[F]) => Aug[F])(x: F)(implicit field: Field[F]): F

  /**
   * Computes a function value and its derivative.
   */
  def diffWithValue[F](f: (Aug[F]) => Aug[F])(x: F)(implicit field: Field[F]): (F, F)

  /**
   * Computes the gradient of a function taking a vector as the argument.
   */
  def grad[F, V[_]](f: (V[Aug[F]]) => Aug[F])(x: V[F])(implicit field: Field[F], concrete: Concrete[V]): V[F]

  /**
   * Computes the value and gradient of a function taking a vector as the argument.
   */
  def gradWithValue[F, V[_]](f: (V[Aug[F]]) => Aug[F])(x: V[F])(implicit field: Field[F], concrete: Concrete[V]): (F, V[F])
}
