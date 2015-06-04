package cml.algebra

/**
 * An endofunctor on the category of pointed types.
 */
trait ZeroEndofunctor[F[_]] extends ZeroFunctor[F] {
  /**
   * Each object (A, zero : A) is mapped to an object (F[A], zero : F[A]).
   */
  def zero[A](implicit a: Zero[A]): F[A]
}

object ZeroEndofunctor {
  class AsZero[F[_], A] (implicit f: ZeroEndofunctor[F], a: Zero[A])
    extends Zero[F[A]] {
    override val zero: F[A] = f.zero[A]
  }

  implicit def asZero[F[_], A](implicit f: ZeroEndofunctor[F], a: Zero[A]) = new AsZero[F, A]

  class Product[F[_], G[_]] (implicit f: ZeroEndofunctor[F], g: ZeroEndofunctor[G])
    extends ZeroFunctor.Product[F, G] with ZeroEndofunctor[({type T[A] = (F[A], G[A])})#T] {
    override def zero[A](implicit a: Zero[A]): (F[A], G[A]) =
      (f.zero, g.zero)
  }

  implicit def product[F[_], G[_]](implicit f: ZeroEndofunctor[F], g: ZeroEndofunctor[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: ZeroEndofunctor[F], g: ZeroEndofunctor[G])
    extends ZeroFunctor.Compose[F, G] with ZeroEndofunctor[({type T[A] = F[G[A]]})#T] {
    override def zero[A](implicit a: Zero[A]): F[G[A]] =
      f.zero[G[A]]
  }

  implicit def compose[F[_], G[_]](implicit f: ZeroEndofunctor[F], g: ZeroEndofunctor[G]) = new Compose[F, G]

  class ConstImpl[C] (default: C) extends ZeroFunctor.ConstImpl[C] with ZeroEndofunctor[({type T[A] = C})#T] {
    override def zero[A](implicit a: Zero[A]): C = default
  }

  def const[C](default: C) = new ConstImpl(default)
}
