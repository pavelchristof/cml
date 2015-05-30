package cml.algebra.traits

import cml.syntax.AdditiveSyntax

/**
 * An abelian group.
 */
trait Additive[T] extends Zero[T] {
  def add(x: T, y: T): T
  def sub(x: T, y: T): T = add(x, neg(y))
  def neg(x: T): T

  val additiveSyntax = new AdditiveSyntax[T] { def F = Additive.this }
}

object Additive {
  class Product[A, B] (implicit a: Additive[A], b: Additive[B])
    extends Zero.Product[A, B] with Additive[(A, B)] {
    override def add(x: (A, B), y: (A, B)): (A, B) = (a.add(x._1, y._1), b.add(x._2, y._2))
    override def sub(x: (A, B), y: (A, B)): (A, B) = (a.sub(x._1, y._1), b.sub(x._2, y._2))
    override def neg(x: (A, B)): (A, B) = (a.neg(x._1), b.neg(x._2))
  }

  implicit def product[A, B](implicit a: Additive[A], b: Additive[B]) = new Product[A, B]

  class Function[A, B] (implicit b: Additive[B])
    extends Zero.Function[A, B] with Additive[(A) => B] {
    override def add(x: (A) => B, y: (A) => B): (A) => B = q => b.add(x(q), y(q))
    override def neg(x: (A) => B): (A) => B = q => b.neg(x(q))
  }

  implicit def function[A, B](implicit b: Additive[B]) = new Function[A, B]
}
