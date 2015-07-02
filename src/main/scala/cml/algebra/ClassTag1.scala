package cml.algebra

import scala.reflect.ClassTag

trait ClassTag1[F[_]] extends Serializable {
  def classTag[A](implicit a: ClassTag[A]): ClassTag[F[A]]
}

object ClassTag1 {
  class AsClassTag[F[_], A] (implicit f: ClassTag1[F], a: ClassTag[A]) extends ClassTag[F[A]] {
    override def runtimeClass: Class[_] = f.classTag[A].runtimeClass
  }

  implicit def asClassTag[F[_], A](implicit f: ClassTag1[F], a: ClassTag[A]) = new AsClassTag[F, A]

  class Product[F[_], G[_]] (implicit f: ClassTag1[F], g: ClassTag1[G])
    extends ClassTag1[({type T[A] = (F[A], G[A])})#T] {
    override implicit def classTag[A](implicit a: ClassTag[A]): ClassTag[(F[A], G[A])] =
      new ClassTag[(F[A], G[A])] {
        override def runtimeClass: Class[_] = classOf[(F[A], G[A])]
      }
  }

  implicit def product[F[_], G[_]](implicit f: ClassTag1[F], g: ClassTag1[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: ClassTag1[F], g: ClassTag1[G])
    extends ClassTag1[({type T[A] = F[G[A]]})#T] {
    override def classTag[A](implicit a: ClassTag[A]): ClassTag[F[G[A]]] =
      f.classTag(g.classTag(a))
  }

  implicit def compose[F[_], G[_]](implicit f: ClassTag1[F], g: ClassTag1[G]) = new Compose[F, G]

  class ConstImpl[C] (implicit c: ClassTag[C]) extends ClassTag1[({type T[A] = C})#T] {
    override def classTag[A](implicit a: ClassTag[A]): ClassTag[C] = c
  }

  def const[C](implicit c: ClassTag[C]) = new ConstImpl[C]
}