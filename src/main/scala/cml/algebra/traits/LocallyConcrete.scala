package cml.algebra.traits

import cml.Enumerate

import scalaz.Functor

trait Covector[V[_]] {
  def apply[A](v: V[A])(implicit field: Analytic[A]): A
}

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
   * Returns a concrete subspace containing v.
   */
  def restrict[A](v: V[A])(implicit field: Field[A]): Concrete[V]

  /**
   * The fundamental property of locally concrete vector spaces is that for any function f on vectors polymorphic in
   * the number type and for each vector v in V, we can factor V as X x Y where X is concrete and f(v) = f(v + y) for
   * all y in Y. This function finds such a subspace X, not necessarily the smallest.
   *
   * It follows that the derivative df(x)/dy = 0 for any y in Y. As such it is enough to consider partial derivatives
   * on X to find the gradient of f.
   *
   * The subspace X does not always depend on the vector v. It only depends on v (and contains restrict(v)) when the
   * function f uses accumulating functions such as sum(), length(), etc. Otherwise the subspace X is constant for
   * all v in V.
   */
  def restrict[A](h: Covector[V])(v: V[A])(implicit field: Field[A]): Concrete[V] =
    throw new NotImplementedError()
}
