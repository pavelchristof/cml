package cml.algebra

trait Zero1[F[_]] extends Apply[F] {
  def zero[A](implicit a: Zero[A]): F[A]
}

object Zero1 {
  class AsZero[F[_], A] (implicit f: Zero1[F], a: Zero[A])
    extends Zero[F[A]] {
    override val zero: F[A] = f.zero
  }

  implicit def asZero[F[_], A](implicit f: Zero1[F], a: Zero[A]) = new AsZero[F, A]

  class Product[F[_], G[_]] (implicit f: Zero1[F], g: Zero1[G])
    extends Apply.Product[F, G] with Zero1[({type T[A] = (F[A], G[A])})#T] {
    override def zero[A](implicit a: Zero[A]): (F[A], G[A]) = (f.zero, g.zero)
  }

  implicit def product[F[_], G[_]](implicit f: Zero1[F], g: Zero1[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: Zero1[F], g: Zero1[G])
    extends Apply.Compose[F, G] with Zero1[({type T[A] = F[G[A]]})#T] {
    override def zero[A](implicit a: Zero[A]): F[G[A]] = f.zero[G[A]]
  }

  implicit def compose[F[_], G[_]](implicit f: Zero1[F], g: Zero1[G]) = new Compose[F, G]
}
