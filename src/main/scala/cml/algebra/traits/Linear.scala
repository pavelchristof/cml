package cml.algebra.traits

/**
 * A functor that maps fields to vector spaces.
 */
trait Linear[F[_]] extends Module[F] {
  def div[A](v: F[A], x: A)(implicit a: Field[A]): F[A] =
    map(v)(a.div(_, x))

  def lerp[A](v: F[A], u: F[A], x: A)(implicit a: Field[A]): F[A] =
    add(mull(x, v), mull(a.sub(a.one, x), u))
}

object Linear {
  import Additive1.asAdditive

  class Product[F[_], G[_]] (implicit f: Linear[F], g: Linear[G])
    extends Module.Product[F, G] with Linear[({type T[A] = (F[A], G[A])})#T] {
    override def div[A](v: (F[A], G[A]), x: A)(implicit a: Field[A]): (F[A], G[A]) =
      (f.div(v._1, x), g.div(v._2, x))

    override def lerp[A](v: (F[A], G[A]), u: (F[A], G[A]), x: A)(implicit a: Field[A]): (F[A], G[A]) =
      (f.lerp(v._1, u._1, x), g.lerp(v._2, u._2, x))
  }

  implicit def product[F[_], G[_]](implicit f: Linear[F], g: Linear[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: Linear[F], g: Linear[G])
    extends Module.Compose[F, G] with Linear[({type T[A] = F[G[A]]})#T] {
    override def div[A](v: F[G[A]], x: A)(implicit a: Field[A]): F[G[A]] =
      f.map(v)(g.div(_, x))

    override def lerp[A](v: F[G[A]], u: F[G[A]], x: A)(implicit a: Field[A]): F[G[A]] =
      f.apply2(v, u)(g.lerp(_, _, x))
  }

  implicit def compose[F[_], G[_]](implicit f: Linear[F], g: Linear[G]) = new Compose[F, G]
}
