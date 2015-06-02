package cml.algebra

import scalaz.Const

/**
 * An endofunctor on the category of pointed types.
 */
trait ZeroFunctor[F[_]] {
  /**
   * Each object (A, zero : A) is mapped to an object (F[A], zero : F[A]).
   */
  def zero[A](implicit a: Zero[A]): F[A]

  /**
   * Lifts a zero preserving function.
   */
  def map[A, B](v: F[A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): F[B]
}

object ZeroFunctor {
  class AsZero[F[_], A] (implicit f: ZeroFunctor[F], a: Zero[A])
    extends Zero[F[A]] {
    override val zero: F[A] = f.zero[A]
  }

  implicit def asZero[F[_], A](implicit f: ZeroFunctor[F], a: Zero[A]) = new AsZero[F, A]

  class Product[F[_], G[_]] (implicit f: ZeroFunctor[F], g: ZeroFunctor[G])
    extends ZeroFunctor[({type T[A] = (F[A], G[A])})#T] {
    override def zero[A](implicit a: Zero[A]): (F[A], G[A]) =
      (f.zero, g.zero)

    override def map[A, B](v: (F[A], G[A]))(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): (F[B], G[B]) =
      (f.map(v._1)(h), g.map(v._2)(h))
  }

  implicit def product[F[_], G[_]](implicit f: ZeroFunctor[F], g: ZeroFunctor[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: ZeroFunctor[F], g: ZeroFunctor[G])
    extends ZeroFunctor[({type T[A] = F[G[A]]})#T] {
    override def zero[A](implicit a: Zero[A]): F[G[A]] =
      f.zero[G[A]]

    override def map[A, B](v: F[G[A]])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): F[G[B]] =
      f.map(v)(g.map(_)(h))
  }

  implicit def compose[F[_], G[_]](implicit f: ZeroFunctor[F], g: ZeroFunctor[G]) = new Compose[F, G]

  class ConstImpl[C] (default: C) extends ZeroFunctor[({type T[A] = C})#T] {
    override def zero[A](implicit a: Zero[A]): C = default

    override def map[A, B](v: C)(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): C = v
  }

  def const[C](default: C) = new ConstImpl(default)
}
