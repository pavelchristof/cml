package cml.models

import cml.algebra.Analytic
import cml.{Model, algebra}
import cml.algebra.traits.Analytic
import shapeless.Nat

case class Identity[V[_]] () extends Model[V, V] {
  val vec = algebra.Vec(Nat(0))
  override type Type[A] = vec.Type[A]

  override implicit val space = vec

  override def apply[F](inst: Type[F])(input: V[F])(implicit f: Analytic[F]): V[F] =
    input
}
