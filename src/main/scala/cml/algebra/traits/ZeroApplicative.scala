package cml.algebra.traits

trait ZeroApplicative[V[_]] extends ZeroApply[V] {
  /**
   * Lifts a value.
   */
  def point[A](x: A)(implicit a: Zero[A]): V[A]
}

object ZeroApplicative {
  import ZeroFunctor.asZero

  class Product[F[_], G[_]] (implicit f: ZeroApplicative[F], g: ZeroApplicative[G])
    extends ZeroApply.Product[F, G] with ZeroApplicative[({type T[A] = (F[A], G[A])})#T] {
    override def point[A](x: A)(implicit a: Zero[A]): (F[A], G[A]) =
      (f.point(x), g.point(x))
  }

  implicit def product[F[_], G[_]](implicit f: ZeroApplicative[F], g: ZeroApplicative[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: ZeroApplicative[F], g: ZeroApplicative[G])
    extends ZeroApply.Compose[F, G] with ZeroApplicative[({type T[A] = F[G[A]]})#T] {
    override def point[A](x: A)(implicit a: Zero[A]): F[G[A]] =
      f.point(g.point(x))
  }

  implicit def compose[F[_], G[_]](implicit f: ZeroApplicative[F], g: ZeroApplicative[G]) = new Compose[F, G]
}