package cml.algebra

import cml.algebra.traits._
import scalaz.{Applicative, Traverse}

object Compose {
  implicit def traverse[F[_], G[_]](implicit f: Traverse[F], g: Traverse[G]) =
    new Traverse[({type T[A] = F[G[A]]})#T] {
    def traverseImpl[App[_], A, B](fa: F[G[A]])(h: (A) => App[B])
        (implicit evidence$1: Applicative[App]): App[F[G[B]]] =
      f.traverse(fa)(g.traverse(_)(h))
  }

  implicit def linear[F[_], G[_]](implicit f: Applicative[F], g: Linear[G]) =
    new Linear[({type T[A] = F[G[A]]})#T] {
      import f.applicativeSyntax._

      override def zero[A](implicit field: Field[A]): F[G[A]] = f.point(g.zero)
      override def add[A](x: F[G[A]], y: F[G[A]])(implicit field: Field[A]): F[G[A]] = ^(x, y) (g.add(_, _))
      override def neg[A](x: F[G[A]])(implicit field: Field[A]): F[G[A]] = x.map(g.neg(_))

      override def mull[A](a: A, v: F[G[A]])(implicit field: Field[A]): F[G[A]] = v.map(g.mull(a, _))
      override def mulr[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] = v.map(g.mulr(_, a))
      override def div[A](v: F[G[A]], a: A)(implicit field: Field[A]): F[G[A]] = v.map(g.div(_, a))
  }
}
