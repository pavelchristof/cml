package cml.algebra.traits

import cml.Enumerate

import scalaz.Functor

/**
 * Potentially infinitely dimensional vector space with a countable (normal) basis and the property that for each
 * vector v a locally concrete vector space contains a (not necessarily minimal) concrete subspace containing v,
 * called the restriction to v.
 */
trait LocallyConcrete[V[_]] extends Normed[V] {
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
   * The (finite) dimension of a vector, equal to the dimension of the restriction to v.
   */
  def dim[A](v: V[A])(implicit field: Field[A]): BigInt = restrict(v).dimFin

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
  def mapLC[A, B](x: V[A])(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): V[B]

  /**
   * Applies a vector of functions to a vector, pointwise. It must hold for each function that f(0) = 0.
   */
  def apLC[A, B](x: V[A])(f: V[A => B])(implicit a: Additive[A], b: Additive[B]): V[B]

  /**
   * Returns the concrete subspace containing v.
   */
  def restrict[A](v: V[A])(implicit field: Field[A]): Concrete[V]
}
