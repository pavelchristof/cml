package cml.models

import cml.{Model, algebra}
import cml.algebra.traits.{Additive, Analytic}
import shapeless.Nat

case class Identity[V[_]] () extends Model[V, V] {
  val vec = algebra.Vector(Nat(0))
  override type Type[A] = vec.Type[A]

  override implicit val space = vec

  override def apply[F](input: V[F])(model: Type[F])(implicit f: Analytic[F]): V[F] =
    input

  override def fill[F](x: => F)(implicit a: Additive[F]): Type[F] =
    vec.point(x)
}
