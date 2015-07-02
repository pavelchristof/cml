package cml.algebra

import scala.reflect.ClassTag

/**
 * An endofunctor.
 */
trait Functor[F[_]] extends ClassTag1[F] {
  /**
   * Lifts a zero preserving function.
   */
  def map[A, B](v: F[A])(h: (A) => B)(implicit a: ClassTag[A], b: ClassTag[B]): F[B]
}

object Functor {
  import ClassTag1.asClassTag

  class Product[F[_], G[_]] (implicit f: Functor[F], g: Functor[G])
    extends ClassTag1.Product[F, G] with Functor[({type T[A] = (F[A], G[A])})#T] {
    override def map[A, B](v: (F[A], G[A]))(h: (A) => B)(implicit a: ClassTag[A], b: ClassTag[B]): (F[B], G[B]) =
      (f.map(v._1)(h), g.map(v._2)(h))
  }

  implicit def product[F[_], G[_]](implicit f: Functor[F], g: Functor[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: Functor[F], g: Functor[G])
    extends ClassTag1.Compose[F, G] with Functor[({type T[A] = F[G[A]]})#T] {
    override def map[A, B](v: F[G[A]])(h: (A) => B)(implicit a: ClassTag[A], b: ClassTag[B]): F[G[B]] =
      f.map(v)(g.map(_)(h))
  }

  implicit def compose[F[_], G[_]](implicit f: Functor[F], g: Functor[G]) = new Compose[F, G]

  class ConstImpl[C] (implicit c: ClassTag[C])
    extends ClassTag1.ConstImpl[C] with Functor[({type T[A] = C})#T] {
    override def map[A, B](v: C)(h: (A) => B)(implicit a: ClassTag[A], b: ClassTag[B]): C = v
  }

  def const[C](implicit c: ClassTag[C]) = new ConstImpl[C]
}
