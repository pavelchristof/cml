package cml.algebra.traits

/**
 * A functor that maps field F to the cartesian space F&#94;n for some natural number n.
 */
trait Cartesian[F[_]] extends Normed[F] with ZeroApplicative[F] {
  /**
   * Dimension of this vector space.
   */
  val dim: Int

  /**
   * Creates a vector with coordinates given by a function.
   */
  def tabulate[A](v: (Key) => A)(implicit a: Zero[A]): F[A]

  /**
   * Creates a new vector from a map. Coefficients for keys not in the map are zero.
   */
  final override def tabulate[A](v: Map[Key, A])(implicit a: Zero[A]): F[A] =
    tabulate((k: Key) => v.getOrElse(k, a.zero))

  /**
   * Returns the whole space.
   */
  final override def restrict(keys: Set[Key]): Subspace[F] =
    new Subspace.WholeSpace[F]()(this)

  /**
   * Lifts a value.
   */
  override def point[A](x: A)(implicit a: Zero[A]): F[A] = tabulate(_ => x)
}
