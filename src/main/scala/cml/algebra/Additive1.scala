package cml.algebra

/**
 * A functor that maps commutative groups to commutative groups.
 */
trait Additive1[F[_]] extends ZeroApply[F] {
  def add[A](x: F[A], y: F[A])(implicit a: Additive[A]): F[A] =
    apply2(x, y)(a.add)
  def sub[A](x: F[A], y: F[A])(implicit a: Additive[A]): F[A] =
    apply2(x, y)(a.sub)
  def neg[A](x: F[A])(implicit a: Additive[A]): F[A] =
    map(x)(a.neg)
}

object Additive1 {
  class AsAdditive[F[_], A] (implicit f: Additive1[F], a: Additive[A])
    extends ZeroFunctor.AsZero[F, A] with Additive[F[A]] {
    override def add(x: F[A], y: F[A]): F[A] = f.add(x, y)
    override def sub(x: F[A], y: F[A]): F[A] = f.sub(x, y)
    override def neg(x: F[A]): F[A] = f.neg(x)
  }

  implicit def asAdditive[F[_], A](implicit f: Additive1[F], a: Additive[A]) = new AsAdditive[F, A]

  class Product[F[_], G[_]] (implicit f: Additive1[F], g: Additive1[G])
    extends ZeroApply.Product[F, G] with Additive1[({type T[A] = (F[A], G[A])})#T] {
    override def add[A](x: (F[A], G[A]), y: (F[A], G[A]))(implicit a: Additive[A]): (F[A], G[A]) =
      (f.add(x._1, y._1), g.add(x._2, y._2))
    override def sub[A](x: (F[A], G[A]), y: (F[A], G[A]))(implicit a: Additive[A]): (F[A], G[A]) =
      (f.sub(x._1, y._1), g.sub(x._2, y._2))
    override def neg[A](x: (F[A], G[A]))(implicit a: Additive[A]): (F[A], G[A]) =
      (f.neg(x._1), g.neg(x._2))
  }

  implicit def product[F[_], G[_]](implicit f: Additive1[F], g: Additive1[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: Additive1[F], g: Additive1[G])
    extends ZeroApply.Compose[F, G] with Additive1[({type T[A] = F[G[A]]})#T] {
    override def add[A](x: F[G[A]], y: F[G[A]])(implicit a: Additive[A]): F[G[A]] =
      f.add(x, y)
    override def sub[A](x: F[G[A]], y: F[G[A]])(implicit a: Additive[A]): F[G[A]] =
      f.sub(x, y)
    override def neg[A](x: F[G[A]])(implicit a: Additive[A]): F[G[A]] =
      f.neg(x)
  }

  implicit def compose[F[_], G[_]](implicit f: Additive1[F], g: Additive1[G]) = new Compose[F, G]
}
