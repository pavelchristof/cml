package cml.models

import cml._
import cml.algebra._

case class Identity[V[_]] () extends Model[V, V] {
  override type Type[A] = Unit

  override implicit val space = Cartesian.Zero

  override def apply[F](inst: Type[F])(input: V[F])(implicit f: Analytic[F]): V[F] =
    input
}
