package cml.models

import cml.algebra
import cml.algebra.traits._
import shapeless.{Nat, _0}

case class Pointwise[V[_]] (
  f: AnalyticMap
) (
  implicit c: Concrete[V]
) extends Model[V, V] {
  override type Type[A] = algebra.Vector[_0, A]

  override implicit val concrete = algebra.Vector(Nat(0))

  def apply[F](input: V[F])(model: Type[F])(implicit field: Analytic[F]): V[F] =
    c.pointwise(f)(input)
}
