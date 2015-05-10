package cml.algebra

import cml.algebra.traits._
import scalaz.{Applicative, Traverse}

object Product {
  def traverse[F[_], G[_]](implicit f: Traverse[F], g: Traverse[G]) =
    new Traverse[({type T[A] = (F[A], G[A])})#T] {
      override def traverseImpl[App[_], A, B](p: (F[A], G[A]))(h: (A) => App[B])
          (implicit a: Applicative[App]): App[(F[B], G[B])] =
        a.tuple2(f.traverse(p._1)(h), g.traverse(p._2)(h))
    }

  def linear[F[_], G[_]](implicit f: Linear[F], g: Linear[G]) =
    new Linear[({type T[A] = (F[A], G[A])})#T] {
      override def zero[A](implicit field: Field[A]): (F[A], G[A]) = (f.zero, g.zero)
      override def add[A](x: (F[A], G[A]), y: (F[A], G[A]))(implicit field: Field[A]): (F[A], G[A]) =
        (f.add(x._1, y._1), g.add(x._2, y._2))
      override def neg[A](x: (F[A], G[A]))(implicit field: Field[A]): (F[A], G[A]) =
        (f.neg(x._1), g.neg(x._2))

      override def mull[A](a: A, v: (F[A], G[A]))(implicit field: Field[A]): (F[A], G[A]) =
        (f.mull(a, v._1), g.mull(a, v._2))
      override def mulr[A](v: (F[A], G[A]), a: A)(implicit field: Field[A]): (F[A], G[A]) =
        (f.mulr(v._1, a), g.mulr(v._2, a))
      override def div[A](v: (F[A], G[A]), a: A)(implicit field: Field[A]): (F[A], G[A]) =
        (f.div(v._1, a), g.div(v._2, a))
  }
}
