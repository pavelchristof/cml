package cml.algebra.traits

/**
 * Vector spaces with norms and a bunch of similar stuff.
 */
trait Normed[F[_]] extends Representable[F] {
  def sum[A](v: F[A])(implicit a: Additive[A]): A

  final def taxicab[A](v: F[A])(implicit a: Analytic[A]): A
    = sum(map(v)(a.abs(_)))
  final def length[A](v: F[A])(implicit a: Analytic[A]): A
    = a.sqrt(quadrance(v))
  final def dist[A](u: F[A], v: F[A])(implicit a: Analytic[A]): A
    = length(sub(u, v))
  final def dot[A](u: F[A], v: F[A])(implicit a: AbelianRing[A]): A
    = sum(apply2(u, v)(a.mul(_, _)))
  final def quadrance[A](v: F[A])(implicit a: AbelianRing[A]): A
    = dot(v, v)
}

object Normed {
  import ZeroFunctor.asZero

  class Product[F[_], G[_]] (implicit override val f: Normed[F], override val g: Normed[G])
    extends Representable.Product[F, G] with Normed[({type T[A] = (F[A], G[A])})#T] {
    override def sum[A](v: (F[A], G[A]))(implicit a: Additive[A]): A =
      a.add(f.sum(v._1), g.sum(v._2))
  }

  implicit def product[F[_], G[_]](implicit f: Normed[F], g: Normed[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit override val f: Normed[F], override val g: Normed[G])
    extends Representable.Compose[F, G] with Normed[({type T[A] = F[G[A]]})#T] {
    override def sum[A](v: F[G[A]])(implicit a: Additive[A]): A =
      f.sum(f.map(v)(g.sum(_)))
  }

  implicit def compose[F[_], G[_]](implicit f: Normed[F], g: Normed[G]) = new Compose[F, G]
}
