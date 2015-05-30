package cml.optimization

import cml.algebra._

object Identity extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: Cartesian[V]): (V[A]) => V[A] = identity
}
