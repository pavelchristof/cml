package cml.algebra

import shapeless.Nat

case class Vec[S <: Nat, A] (
  get: Array[A]
) extends Serializable {
  override def toString: String = get.toVector.toString()
}
