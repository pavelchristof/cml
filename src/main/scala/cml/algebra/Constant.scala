package cml.algebra

import cml.Enumerate
import cml.algebra.traits._

import scalaz.{Functor, Monoid}

/**
 * Constant functor (and a 0 dimensional vector space).
 */
case class Constant[C, A] (value: C)

object Constant {
  def functor[C]: Functor[({type T[A] = Constant[C, A]})#T] = new Functor[({type T[A] = Constant[C, A]})#T] {
    override def map[A, B](fa: Constant[C, A])(f: (A) => B): Constant[C, B] =
      Constant(fa.value)
  }

  def concrete[C](implicit m: Monoid[C]): Concrete[({type T[A] = Constant[C, A]})#T] =
    new Concrete[({type T[A] = Constant[C, A]})#T] {
      override type Index = Void
      override def enumerateIndex: Enumerate[Void] = Enumerate.void
      override val dimFin: BigInt = 0

      override def tabulate[A](h: (Void) => A): Constant[C, A] = Constant(m.zero)
      override def index[A](v: Constant[C, A])(i: Void): A = i.asInstanceOf[A]

      override def map[A, B](fa: Constant[C, A])(f: (A) => B): Constant[C, B] =
        Constant(fa.value)
      override def point[A](a: => A): Constant[C, A] =
        Constant(m.zero)
      override def ap[A, B](x: => Constant[C, A])(f: => Constant[C, (A) => B]): Constant[C, B] =
        Constant(m.append(x.value, f.value))
    }
}
