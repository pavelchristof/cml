package cml.algebra

import cml.Enumerate
import cml.algebra.traits.Concrete

object Scalar extends Concrete[({type T[A] = A})#T] {
  type Type[A] = A

  override type Index = Unit

  override def enumerateIndex: Enumerate[Index] = Enumerate.unit

  override val dimFin: BigInt = 1

  override def tabulate[A](h: (Index) => A): A = h()

  override def index[A](v: A)(i: Index): A = v
}
