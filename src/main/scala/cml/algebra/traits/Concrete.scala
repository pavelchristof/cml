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
   * The (normal) basis for this vector space.
   */
  override def basis[A](i: Index)(implicit field: Field[A]): V[A] =
    tabulate(j => if (i == j) field.one else field.zero)

  /**
   * Construct a vector from coefficients of the basis vectors.
   */
  def tabulate[A](h: (Index) => A): V[A]

  /**
   * Find the coefficient of a basis vector.
   */
  def index[A](v: V[A])(i: Index): A

  override def zero[F](implicit f: Additive[F]): V[F] =
    point(f.zero)
  override def add[F](x: V[F], y: V[F])(implicit f: Additive[F]): V[F] =
    apply2(x, y)(f.add)
  override def neg[F](x: V[F])(implicit f: Additive[F]): V[F] =
    map(x)(f.neg)

  override def mull[F](a: F, v: V[F])(implicit f: Field[F]): V[F] =
    map(v)(f.mul(a, _))
  override def mulr[F](v: V[F], a: F)(implicit f: Field[F]): V[F] =
    map(v)(f.mul(_, a))
  override def div[F](v: V[F], a: F)(implicit f: Field[F]): V[F] =
    map(v)(f.div(_, a))

  override def sum[A](v: V[A])(implicit a: Additive[A]): A =
    foldRight(v, a.zero)(a.add(_, _))
  override def taxicab[A](v: V[A])(implicit a: Analytic[A]): A =
    foldRight(v, a.zero){ case (x, y) => a.add(a.neg(x), y) }
  override def dot[A](u: V[A], v: V[A])(implicit f: Field[A]): A =
    sum(apply2(u, v)(f.mul))

  /**
   * Construct a vector from coefficients of the basis vectors.
   */
  final override def tabulateLC[A](h: Map[Index, A])(implicit a: Additive[A]): V[A] =
    tabulate(i => h.applyOrElse[Index, A](i, _ => a.zero))

  /**
   * Find the coefficient of a basis vector.
   */
  final override def indexLC[A](v: V[A])(i: Index)(implicit a: Additive[A]): A =
    index(v)(i)

  /**
   * Maps the vector with a function f. It must hold that f(0) = 0.
   */
  final override def mapLC[A, B](x: V[A])(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): V[B] =
    map(x)(f)

  /**
   * Applies a vector of functions to a vector, pointwise. It must hold that f(0) = 0.
   */
  final override def apLC[A, B](x: V[A])(f: V[(A) => B])(implicit a: Additive[A], b: Additive[B]): V[B] =
    ap(x)(f)

  /**
   * Returns the concrete subspace containing v.
   */
  final override def restrict[A](v: V[A])(implicit field: Field[A]): Concrete[V] = this

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
  override def restrict[A](h: V[A] => A)(v: V[A])(implicit a: Additive[A]): Concrete[V] = this

  override def map[A, B](v: V[A])(f: (A) => B): V[B] = {
    val coeff = index(v)_
    tabulate(i => f(coeff(i)))
  }

  override def point[A](a: => A): V[A] = tabulate(_ => a)

  override def ap[A, B](x: => V[A])(f: => V[(A) => B]): V[B] = {
    val xi = index(x)_
    val fi = index(f)_
    tabulate(i => fi(i)(xi(i)))
  }

  override def foldMap[A, B](fa: V[A])(f: (A) => B)(implicit F: Monoid[B]): B =
    enumerateIndex.enumerate.map(i => f(index(fa)(i))).fold(F.zero)(F.append(_, _))

  override def foldRight[A, B](fa: V[A], z: => B)(f: (A, => B) => B): B =
    enumerateIndex.enumerate.map(i => index(fa)(i)).foldRight(z)(f(_, _))
}
