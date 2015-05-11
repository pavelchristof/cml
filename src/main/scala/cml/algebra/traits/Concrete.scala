package cml.algebra.traits

import scalaz.{Monoid, Foldable, Applicative}

/**
 * Finitely dimensional vector spaces with a canonical normal basis.
 */
trait Concrete[V[_]] extends LocallyConcrete[V] with Applicative[V] with Foldable[V] {
  /**
   * The (finite) dimension of this vector space.
   */
  val dimFin: BigInt

  /**
   * The (finite) dimension of this vector space.
   */
  final override val dim: Option[BigInt] = Some(dimFin)

  /**
   * Construct a vector from coefficients of the basis vectors.
   */
  def tabulate[A](h: (Index) => A): V[A]

  /**
   * Construct a vector from coefficients of the basis vectors.
   */
  override def tabulate[A](h: Map[Index, A])(implicit a: Additive[A]): V[A] =
    tabulate(i => h.applyOrElse[Index, A](i, _ => a.zero))

  /**
   * The (normal) basis for this vector space.
   */
  override def basis[A](i: Index)(implicit field: Field[A]): V[A] =
    tabulate(j => if (i == j) field.one else field.zero)

  /**
   * Returns the concrete subspace containing v.
   */
  final override def restrict[A](v: V[A])(implicit field: Field[A]): Concrete[V] = this

  /**
   * Applies a function, possibly changing the underlying field.
   */
  override def map[A, B](v: V[A])(f: (A) => B): V[B] = {
    val coeff = index(v)_
    tabulate(i => f(coeff(i)))
  }

  /**
   * Sets all coefficients to some value.
   */
  override def point[A](a: => A): V[A] = tabulate(_ => a)

  /**
   * Applies functions pointwise.
   */
  override def ap[A, B](x: => V[A])(f: => V[(A) => B]): V[B] = {
    val xi = index(x)_
    val fi = index(f)_
    tabulate(i => fi(i)(xi(i)))
  }

  /**
   * Applies a function pointwise on the coordinates of the vector.
   *
   * Restricted to natural maps.
   */
  def pointwise[A](g: AnalyticMap)(v: V[A])(implicit an: Analytic[A]): V[A] = {
    val coeff = index(v)_
    tabulate(i => g(coeff(i)))
  }

  /**
   * Sums all the coordinates.
   */
  override def sum[A](v: V[A])(implicit a: Additive[A]): A = foldRight(v, a.zero)(a.add(_, _))

  override def foldMap[A, B](fa: V[A])(f: (A) => B)(implicit F: Monoid[B]): B =
    enumerateIndex.enumerate.map(i => f(index(fa)(i))).fold(F.zero)(F.append(_, _))

  override def foldRight[A, B](fa: V[A], z: => B)(f: (A, => B) => B): B =
    enumerateIndex.enumerate.map(i => index(fa)(i)).foldRight(z)(f(_, _))
}
