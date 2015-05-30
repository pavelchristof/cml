package cml.algebra.traits

/**
 * Finitely dimensional vector spaces with a canonical normal basis.
 */
trait Concrete[V[_]] extends Normed[V] {
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
  def tabulate[A](h: (Index) => A)(implicit additive: Additive[A]): V[A]

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

  final override def restrict(keys: Set[Index]): Subspace[V] =
    Subspace.identity[V](this)

  def point[A](a: => A)(implicit additive: Additive[A]): V[A] = tabulate(_ => a)
}
