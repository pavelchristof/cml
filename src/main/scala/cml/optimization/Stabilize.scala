package cml.optimization

import cml.algebra.traits.{LocallyConcrete, Floating}

case class Stabilize[V[_], A] (
  implicit fl: Floating[A],
  space: LocallyConcrete[V]
) extends GradTransformer[V, A] {
  override def apply(grad: V[A]): V[A] =
    space.mapLC(grad)(x => if (fl.isNaN(x)) fl.zero else x)
}
