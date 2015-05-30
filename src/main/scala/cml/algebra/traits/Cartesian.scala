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

object Cartesian {
  import ZeroFunctor.asZero

  class Product[F[_], G[_]] (implicit override val f: Cartesian[F], override val g: Cartesian[G])
    extends Normed.Product[F, G] with Cartesian[({type T[A] = (F[A], G[A])})#T] {
    override val dim: Int = f.dim + g.dim

    override def tabulate[A](v: (Either[f.Key, g.Key]) => A)(implicit a: Zero[A]): (F[A], G[A]) =
      (f.tabulate(k => v(Left(k))), g.tabulate(k => v(Right(k))))

    override def point[A](x: A)(implicit a: Zero[A]): (F[A], G[A]) =
      (f.point(x), g.point(x))
  }

  implicit def product[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit override val f: Cartesian[F], override val g: Cartesian[G])
    extends Normed.Compose[F, G] with Cartesian[({type T[A] = F[G[A]]})#T] {
    override val dim: Int = f.dim * g.dim

    override def tabulate[A](v: ((f.Key, g.Key)) => A)(implicit a: Zero[A]): F[G[A]] =
      f.tabulate(i => g.tabulate(j => v((i, j))))

    override def point[A](x: A)(implicit a: Zero[A]): F[G[A]] =
      f.point(g.point(x))
  }

  implicit def compose[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new Compose[F, G]
}