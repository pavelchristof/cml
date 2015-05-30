package cml.optimization

import cml.algebra._

case class Scale (scale: Double) extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: Cartesian[V]): (V[A]) => V[A] =
    space.mull(fl.fromDouble(scale), _)
}
