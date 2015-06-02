package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class Identity[V[_]] () extends Model[V, V] {
  override type Type[A] = Unit

  override implicit val space = Cartesian.Zero

  override def apply[F](inst: Type[F])(input: V[F])(implicit f: Analytic[F]): V[F] =
    input

  override def applySubspace[A](subspace: WholeSpace[Type], inst: Any)(input: V[A])
      (implicit a: Analytic[A]): V[A] = input
}
