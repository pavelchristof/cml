package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class Softmax[V[_]] (
  implicit vs: Normed[V]
) extends ParameterlessModel[V, V] {
  def apply[A](input: V[A])(implicit a: Analytic[A]): V[A] = {
    val expd = vs.map(input)(a.exp)
    val total = vs.sum(expd)
    vs.map(expd)(a.div(_, total))
  }
}
