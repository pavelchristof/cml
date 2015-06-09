package cml.models

import cml.ParameterlessModel
import cml.algebra.{Analytic, Monoid1}

case class MonoidEmpty[In[_], V[_]] (
  implicit monoid1: Monoid1[V]
) extends ParameterlessModel[In, V] {
  def apply[A](in: In[A])(implicit a: Analytic[A]): V[A] = monoid1.empty
}
