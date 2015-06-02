package cml.algebra

import shapeless.Nat
import shapeless.ops.nat.ToInt

trait RuntimeNat extends Serializable {
  type Type <: Nat
  def apply(): ToInt[Type]
}

object RuntimeNat {
  def apply(n: Int): RuntimeNat = new RuntimeNat {
    assert(n >= 0)
    override type Type = Nat
    override def apply(): ToInt[Type] = new ToInt[Type] {
      override def apply(): Int = n
    }
  }
}

