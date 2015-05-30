package cml.algebra

/**
 * A commutative ring.
 */
trait AbelianRing[A] extends Ring[A]

object AbelianRing {
  class Product[A, B] (implicit a: AbelianRing[A], b: AbelianRing[B])
    extends Ring.Product[A, B] with AbelianRing[(A, B)]

  implicit def product[A, B](implicit a: AbelianRing[A], b: AbelianRing[B]) = new Product[A, B]

  class Function[A, B] (implicit b: AbelianRing[B])
    extends Ring.Function[A, B] with AbelianRing[(A) => B]

  implicit def function[A, B](implicit b: AbelianRing[B]) = new Function[A, B]
}
