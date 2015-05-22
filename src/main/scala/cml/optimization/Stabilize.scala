package cml.optimization

import cml.algebra.traits._

object Stabilize extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: LocallyConcrete[V]): (V[A]) => V[A] =
    space.mapLC(_)(x => if (fl.isNaN(x)) fl.zero else x)
}
