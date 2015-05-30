package cml.algebra

import cml.syntax.RingSyntax

/**
 * A (not necessarily commutative) ring.
 */
trait Ring[T] extends Additive[T] {
  def mul(x: T, y: T): T
  val one: T

  val ringSyntax = new RingSyntax[T] { def F = Ring.this }
}

object Ring {
  class Product[A, B] (implicit a: Ring[A], b: Ring[B])
    extends Additive.Product[A, B] with Ring[(A, B)] {
    override def mul(x: (A, B), y: (A, B)): (A, B) = (a.mul(x._1, y._1), b.mul(x._2, y._2))
    override val one: (A, B) = (a.one, b.one)
  }

  implicit def product[A, B](implicit a: Ring[A], b: Ring[B]) = new Product[A, B]

  class Function[A, B] (implicit b: Ring[B])
    extends Additive.Function[A, B] with Ring[(A) => B] {
    override def mul(x: (A) => B, y: (A) => B): (A) => B = q => b.mul(x(q), y(q))
    override val one: (A) => B = _ => b.one
  }

  implicit def function[A, B](implicit b: Ring[B]) = new Function[A, B]
}
