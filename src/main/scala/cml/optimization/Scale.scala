package cml.optimization

import cml.algebra.traits._

case class Scale[V[_], A] (
  scale: A
) (
  implicit f: Field[A],
  space: Linear[V]
) extends GradTrans[V, A] {
  override def apply(grad: V[A]): V[A] = space.mull(scale, grad)
}
