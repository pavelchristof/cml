package cml.algebra

import scala.reflect.ClassTag

trait Applicative[V[_]] extends Apply[V] {
  /**
   * Lifts a value.
   */
  def point[A](x: A)(implicit a: ClassTag[A]): V[A]
}

object Applicative {
  class Product[F[_], G[_]] (implicit f: Applicative[F], g: Applicative[G])
    extends Apply.Product[F, G] with Applicative[({type T[A] = (F[A], G[A])})#T] {
    override def point[A](x: A)(implicit a: ClassTag[A]): (F[A], G[A]) =
      (f.point(x), g.point(x))
  }

  implicit def product[F[_], G[_]](implicit f: Applicative[F], g: Applicative[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: Applicative[F], g: Applicative[G])
    extends Apply.Compose[F, G] with Applicative[({type T[A] = F[G[A]]})#T] {
    override def point[A](x: A)(implicit a: ClassTag[A]): F[G[A]] =
      f.point(g.point(x))(g.classTag)
  }

  implicit def compose[F[_], G[_]](implicit f: Applicative[F], g: Applicative[G]) = new Compose[F, G]
}
