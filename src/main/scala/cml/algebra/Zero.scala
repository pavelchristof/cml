package cml.algebra

import scala.reflect.ClassTag

trait Zero[A] extends ClassTag[A] {
  val zero: A

  override def runtimeClass: Class[_] = zero.getClass
}

object Zero {
  class Product[A, B] (implicit a: Zero[A], b: Zero[B]) extends Zero[(A, B)] {
    override val zero: (A, B) = (a.zero, b.zero)
  }

  implicit def product[A, B](implicit a: Zero[A], b: Zero[B]): Zero[(A, B)] = new Product

  class Function[A, B] (implicit b: Zero[B]) extends Zero[(A) => B] {
    override val zero: (A) => B = _ => b.zero
  }

  implicit def function[A, B](implicit b: Zero[B]): Zero[(A) => B] = new Function[A, B]
}
