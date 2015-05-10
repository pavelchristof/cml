package cml.models

import cml.algebra.traits.Analytic
import cml.algebra.Vector
import shapeless.{Nat, _0}

case class Identity[V[_]] () extends Model[V, V] {
  override type Type[A] = Vector[_0, A]

  override implicit val linear = Vector(Nat(0))
  override implicit val functor = Vector(Nat(0))

  override def apply[F](input: V[F])(model: Type[F])(implicit f: Analytic[F]): V[F] = input
}
