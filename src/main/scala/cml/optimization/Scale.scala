package cml.optimization

import cml.algebra.Floating
import cml.algebra.traits._

case class Scale (scale: Double) extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: Concrete[V]): (V[A]) => V[A] =
    space.mull(fl.fromDouble(scale), _)
}
