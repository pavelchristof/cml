package cml.models

import cml.algebra
import cml.algebra.traits.Analytic
import shapeless.{Nat, _0}

case class Identity[V[_]] () extends Model[V, V] {
  override type Type[A] = algebra.Vector[_0, A]

  override implicit val linear = algebra.Vector(Nat(0))
  override implicit val traverse = algebra.Vector(Nat(0))

  override def apply[F](input: V[F])(model: Type[F])(implicit f: Analytic[F]): V[F] = input
}
