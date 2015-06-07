package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class Pointwise[V[_]] (
  f: AnalyticMap
) (implicit
  c: Cartesian[V]
) extends ParameterlessModel[V, V] {
  def apply[A](input: V[A])(implicit a: Analytic[A]): V[A] =
    c.map(input)(f(_))
}
