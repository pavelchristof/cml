package cml.models

import cml._
import cml.algebra._

final case class Duplicate[V[_]] () extends ParameterlessModel[V, ({type T[A] = (V[A], V[A])})#T] {
  override def apply[F](input: V[F])(implicit f: Analytic[F]): (V[F], V[F]) =
    (input, input)
}
