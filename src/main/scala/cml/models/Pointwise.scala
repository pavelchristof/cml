package cml.models

import cml.algebra
import cml.algebra.traits._
import shapeless.{Nat, _0}

case class Pointwise[V[_]] (
  f: AnalyticMap
) (
  implicit concrete: Concrete[V]
) extends Model[V, V] {
  override type Type[A] = algebra.Vector[_0, A]

  override implicit val traverse = algebra.Vector(Nat(0))
  override implicit val linear = algebra.Vector(Nat(0))

  def apply[F](input: V[F])(model: Type[F])(implicit field: Analytic[F]): V[F] =
    concrete.pointwise(f)(input)
}
