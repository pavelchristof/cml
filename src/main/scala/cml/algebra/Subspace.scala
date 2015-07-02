package cml.algebra

/**
 * A cartesian subspace of F.
 */
trait Subspace[F[_]] extends Serializable {
  type Type[A]

  /**
   * Projects a value to the subspace.
   */
  def project[A](v: F[A])(implicit a: Zero[A]): Type[A]

  /**
   * Injects a value from the subspace.
   */
  def inject[A](v: Type[A])(implicit a: Zero[A]): F[A]

  /**
   * Witness that the subspace is cartesian.
   */
  implicit val space: Cartesian[Type]
}

object Subspace {
  import Zero1.asZero

  class Product[F[_], G[_], FS <: Subspace[F], GS <: Subspace[G]] (val _1: FS, val _2: GS)
    extends Subspace[({type T[A] = (F[A], G[A])})#T] {
    override type Type[A] = (_1.Type[A], _2.Type[A])

    override def project[A](v: (F[A], G[A]))(implicit a: Zero[A]): Type[A] =
      (_1.project(v._1), _2.project(v._2))

    override def inject[A](v: (_1.Type[A], _2.Type[A]))(implicit a: Zero[A]): (F[A], G[A]) =
      (_1.inject(v._1), _2.inject(v._2))

    override implicit val space: Cartesian[({type T[A] = (_1.Type[A], _2.Type[A])})#T] =
      Cartesian.product[_1.Type, _2.Type](_1.space, _2.space)
  }

  class Compose[F[_], G[_], FS <: Subspace[F], GS <: Subspace[G]] (val f: FS, val g: GS)
      (implicit fs: Zero1[F], gs: Zero1[G])
    extends Subspace[({type T[A] = F[G[A]]})#T] {
    override type Type[A] = f.Type[g.Type[A]]

    implicit val fss = f.space
    implicit val gss = g.space

    override def project[A](v: F[G[A]])(implicit a: Zero[A]): Type[A] =
      f.space.map(f.project(v))(g.project(_))

    override def inject[A](v: f.Type[g.Type[A]])(implicit a: Zero[A]): F[G[A]] =
      f.inject(f.space.map(v)(g.inject(_)))

    override implicit val space: Cartesian[({type T[A] = f.Type[g.Type[A]]})#T] =
      Cartesian.compose[f.Type, g.Type]

  }

  class WholeSpace[F[_]] (implicit f: Cartesian[F]) extends Subspace[F] {
    override type Type[A] = F[A]

    override def project[A](v: F[A])(implicit a: Zero[A]): F[A] = v

    override def inject[A](v: F[A])(implicit a: Zero[A]): F[A] = v

    override implicit val space: Cartesian[F] = f
  }
}
