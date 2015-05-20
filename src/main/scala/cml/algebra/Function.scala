package cml.algebra

import cml.algebra.traits.Additive

object Function {
  class FunctionAdditive[A, B](implicit a: Additive[B]) extends Additive[(A) => B] {
    override val zero: (A) => B = _ => a.zero
    override def add(f: (A) => B, g: (A) => B): (A) => B = x => a.add(f(x), g(x))
    override def neg(f: (A) => B): (A) => B = x => a.neg(f(x))

    override def runtimeClass: Class[_] = zero.getClass
  }

  implicit def additive[A, B](implicit a: Additive[B]): Additive[(A) => B] =
    new FunctionAdditive[A, B]
}
