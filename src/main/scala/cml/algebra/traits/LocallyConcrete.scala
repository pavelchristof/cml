package cml.algebra.traits

import cml.Enumerate

import scalaz.{Apply}

/**
 * Potentially infinitely dimensional vector space with a countable (normal) basis and the property that for each
 * vector v a locally concrete vector space contains a (not necessarily minimal) concrete subspace containing v,
 * called the restriction to v.
 */
trait LocallyConcrete[V[_]] extends Normed[V] with Apply[V] {
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
   * Construct a vector from coefficients of the basis vectors.
   */
  def tabulate[A](h: Map[Index, A])(implicit a: Additive[A]): V[A]

  /**
   * Find the coefficient of a basis vector.
   */
  def index[A](v: V[A])(i: Index): A

  /**
   * The (normal) basis for this vector space.
   */
  def basis[A](i: Index)(implicit field: Field[A]): V[A]

  /**
   * Returns the concrete subspace containing v.
   */
  def restrict[A](v: V[A])(implicit field: Field[A]): Concrete[V]
}
