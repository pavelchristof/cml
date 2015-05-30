package cml.algebra

/**
 * A functor that maps abelian rings to modules.
 */
trait Module[F[_]] extends Additive1[F] {
  def mull[A](x: A, v: F[A])(implicit a: AbelianRing[A]): F[A] =
    map(v)(a.mul(x, _))
  def mulr[A](v: F[A], x: A)(implicit a: AbelianRing[A]): F[A] =
    map(v)(a.mul(_, x))
}

object Module {
  import Additive1.asAdditive

  class Product[F[_], G[_]] (implicit f: Module[F], g: Module[G])
    extends Additive1.Product[F, G] with Module[({type T[A] = (F[A], G[A])})#T] {
    override def mull[A](x: A, v: (F[A], G[A]))(implicit a: AbelianRing[A]): (F[A], G[A]) =
      (f.mull(x, v._1), g.mull(x, v._2))
    override def mulr[A](v: (F[A], G[A]), x: A)(implicit a: AbelianRing[A]): (F[A], G[A]) =
      (f.mulr(v._1, x), g.mulr(v._2, x))
  }

  implicit def product[F[_], G[_]](implicit f: Module[F], g: Module[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: Module[F], g: Module[G])
    extends Additive1.Compose[F, G] with Module[({type T[A] = F[G[A]]})#T] {
    override def mull[A](x: A, v: F[G[A]])(implicit a: AbelianRing[A]): F[G[A]] =
      f.map(v)(g.mull(x, _))
    override def mulr[A](v: F[G[A]], x: A)(implicit a: AbelianRing[A]): F[G[A]] =
      f.map(v)(g.mulr(_, x))
  }

  implicit def compose[F[_], G[_]](implicit f: Module[F], g: Module[G]) = new Compose[F, G]
}
