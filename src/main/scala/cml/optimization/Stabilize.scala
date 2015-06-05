package cml.optimization

import cml.algebra._

object Stabilize extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: Representable[V]): (V[A]) => V[A] =
    space.map(_)(x => if (fl.isNaN(x)) fl.zero else x)
}
