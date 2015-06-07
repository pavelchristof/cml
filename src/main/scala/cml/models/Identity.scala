package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class Identity[V[_]] () extends ParameterlessModel[V, V] {
  override def apply[F](input: V[F])(implicit f: Analytic[F]): V[F] =
    input
}
