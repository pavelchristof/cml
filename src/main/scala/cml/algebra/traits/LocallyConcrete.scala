package cml.algebra.traits

import cml.Enumerate

/**
 * Potentially infinitely (but countably) dimensional vector space.
 */
trait LocallyConcrete[V[_]] extends Linear[V] {
  /**
   * A countable or finite set indexing the basis.
   */
  type Index

  /**
   * The index must be recursively enumerable.
   */
  def enumerateIndex: Enumerate[Index]

  /**
   * Dimension of the vector space, possibly infinite.
   */
  def dim: Option[BigInt] = enumerateIndex.count

  /**
   * The (normal) basis for this vector space.
   */
  def basis[A](i: Index)(implicit field: Field[A]): V[A]

  /**
   * Construct a vector from coefficients of the basis vectors.
   */
  def tabulateLC[A](h: Map[Index, A])(implicit a: Additive[A]): V[A]

  /**
   * Find the coefficient of a basis vector.
   */
  def indexLC[A](v: V[A])(i: Index)(implicit a: Additive[A]): A

  /**
   * Maps the vector with a function f. It must hold that f(0) = 0.
   */
  def map[A, B](x: V[A])(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): V[B]

  /**
   * Applies a vector of functions to a vector, pointwise. It must hold for each function that f(0) = 0.
   */
  def ap[A, B](x: V[A])(f: V[A => B])(implicit a: Additive[A], b: Additive[B]): V[B]

  /**
   * Applies a binary function pointwise. It must hold that f(0, 0) = 0.
   */
  def apply2[A, B, C](x: V[A], y: V[B])(f: (A, B) => C)(implicit a: Additive[A], b: Additive[B], c: Additive[C]): V[C]

  /**
   * Returns a finite subspace restricted to a set of keys.
   */
  def restrict(keys: Set[Index]): Subspace[V]
}
