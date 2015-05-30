package cml.optimization

import cml.algebra.Floating
import cml.algebra.traits._

object Stabilize extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: Concrete[V]): (V[A]) => V[A] =
    space.map(_)(x => if (fl.isNaN(x)) fl.zero else x)
}
