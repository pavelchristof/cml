package cml.optimization

import cml.algebra.traits._

object Identity extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: LocallyConcrete[V]): (V[A]) => V[A] = identity
}
