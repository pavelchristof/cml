package cml.algebra.traits

import cml.Enumerate

/**
 * Potentially infinitely dimensional vector space with a countable (normal) basis and the property that for each
 * vector v a locally concrete vector space contains a concrete subspace including v and closed under projection on
 * the basis vectors, called the restriction to v.
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
  def dim[A](v: V[A])(implicit field: Field[A]): BigInt = restrict(v).concrete.dimFin

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
   * Applies a binary function pointwise. If must hold that f(0, 0) = 0.
   */
  def apply2LC[A, B, C](x: V[A], y: V[B])(f: (A, B) => C)(implicit a: Additive[A], b: Additive[B], c: Additive[C]): V[C]

  /**
   * Returns a concrete subspace containing v.
   */
  def restrict[A](v: V[A])(implicit field: Field[A]): Subspace[V]

  /**
   * The subspace X does not always depend on the vector v. It only depends on v (and contains restrict(v)) when the
   * function f uses accumulating functions such as sum(), length(), etc. Otherwise the subspace X is constant for
   * all v in V.
   *
   * TODO: figure out what does it really do.
   */
  def restrict[A](h: (V[A]) => A)(v: V[A])(implicit a: Additive[A]): Subspace[V]
}
