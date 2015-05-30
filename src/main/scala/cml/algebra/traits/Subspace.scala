package cml.algebra.traits

/**
 * A cartesian subspace of F.
 */
trait Subspace[F[_]] {
  type Type[A]

  /**
   * Injects a value from the subspace.
   */
  def inject[A](u: Type[A])(implicit a: Zero[A]): F[A]

  /**
   * Projects a value to the subspace.
   */
  def project[A](v: F[A])(implicit a: Zero[A]): Type[A]

  /**
   * Witness that the subspace is cartesian.
   */
  implicit val space: Cartesian[Type]
}

object Subspace {
  import ZeroFunctor.asZero

  class Product[F[_], G[_]] (val f: Subspace[F], val g: Subspace[G])
    extends Subspace[({type T[A] = (F[A], G[A])})#T] {
    override type Type[A] = (f.Type[A], g.Type[A])

    override def inject[A](u: Type[A])(implicit a: Zero[A]): (F[A], G[A]) =
      (f.inject(u._1), g.inject(u._2))

    override def project[A](v: (F[A], G[A]))(implicit a: Zero[A]): Type[A] =
      (f.project(v._1), g.project(v._2))

    override implicit val space: Representable[({type T[A] = (f.Type[A], g.Type[A])})#T] =
      Representable.product[f.Type, g.Type](f.space, g.space)
  }

  class Compose[F[_], G[_]] (val f: Subspace[F], val g: Subspace[G])
      (implicit fs: ZeroFunctor[F], gs: ZeroFunctor[G])
    extends Subspace[({type T[A] = F[G[A]]})#T] {
    override type Type[A] = f.Type[g.Type[A]]

    implicit val gss = g.space

    override def inject[A](u: Type[A])(implicit a: Zero[A]): F[G[A]] =
      f.inject(f.space.map[g.Type[A], G[A]](u)(g.inject(_)))

    override def project[A](v: F[G[A]])(implicit a: Zero[A]): Type[A] =
      f.space.map(f.project(v))(g.project(_))
  }

  class WholeSpace[F[_]] (implicit f: Cartesian[F]) extends Subspace[F] {
    override type Type[A] = F[A]

    override def inject[A](u: F[A])(implicit a: Zero[A]): F[A] = u

    override def project[A](v: F[A])(implicit a: Zero[A]): F[A] = v

    override implicit val space: Cartesian[F] = f
  }
}
