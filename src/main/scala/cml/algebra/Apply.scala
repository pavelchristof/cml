package cml.algebra

import scala.reflect.ClassTag

trait Apply[V[_]] extends Functor[V] {
  /**
   * Zips two "vectors".
   */
  def zip[A, B](x: V[A], y: V[B])(implicit a: ClassTag[A], b: ClassTag[B]): V[(A, B)] =
    apply2(x, y)((_, _))

  /**
   * Applies functions pointwise.
   */
  def ap[A, B](x: V[A])(h: V[(A) => B])(implicit a: ClassTag[A], b: ClassTag[B]): V[B] =
    apply2(x, h)((y, f) => f(y))

  /**
   * Zips two "vectors" and applies a function pointwise.
   */
  def apply2[A, B, C](x: V[A], y: V[B])(h: (A, B) => C)
    (implicit a: ClassTag[A], b: ClassTag[B], c: ClassTag[C]): V[C]
}

object Apply {
  import ClassTag1.asClassTag

  class Product[F[_], G[_]] (implicit f: Apply[F], g: Apply[G])
    extends Functor.Product[F, G] with Apply[({type T[A] = (F[A], G[A])})#T] {
    override def zip[A, B](x: (F[A], G[A]), y: (F[B], G[B]))
        (implicit a: ClassTag[A], b: ClassTag[B]): (F[(A, B)], G[(A, B)]) =
      (f.zip(x._1, y._1), g.zip(x._2, y._2))

    override def ap[A, B](x: (F[A], G[A]))(h: (F[(A) => B], G[(A) => B]))
        (implicit a: ClassTag[A], b: ClassTag[B]): (F[B], G[B]) =
      (f.ap(x._1)(h._1), g.ap(x._2)(h._2))

    override def apply2[A, B, C](x: (F[A], G[A]), y: (F[B], G[B]))(h: (A, B) => C)
        (implicit a: ClassTag[A], b: ClassTag[B], c: ClassTag[C]): (F[C], G[C]) =
      (f.apply2(x._1, y._1)(h), g.apply2(x._2, y._2)(h))
  }

  implicit def product[F[_], G[_]](implicit f: Apply[F], g: Apply[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: Apply[F], g: Apply[G])
    extends Functor.Compose[F, G] with Apply[({type T[A] = F[G[A]]})#T] {
    override def zip[A, B](x: F[G[A]], y: F[G[B]])(implicit a: ClassTag[A], b: ClassTag[B]): F[G[(A, B)]] =
      f.map(f.zip(x, y))(ab => g.zip(ab._1, ab._2))

    override def ap[A, B](x: F[G[A]])(h: F[G[(A) => B]])(implicit a: ClassTag[A], b: ClassTag[B]): F[G[B]] =
      f.ap[G[A], G[B]](x)(f.map(h)(gab => ga => g.ap(ga)(gab)))

    override def apply2[A, B, C](x: F[G[A]], y: F[G[B]])(h: (A, B) => C)
        (implicit a: ClassTag[A], b: ClassTag[B], c: ClassTag[C]): F[G[C]] =
      f.apply2(x, y)(g.apply2(_, _)(h))
  }

  implicit def compose[F[_], G[_]](implicit f: Apply[F], g: Apply[G]) = new Compose[F, G]
}
