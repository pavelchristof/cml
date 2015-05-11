package cml.models

import cml.algebra
import cml.algebra.traits._
import shapeless.Nat

case class Pointwise[V[_]] (
  f: AnalyticMap
) (
  implicit c: Concrete[V]
) extends Model[V, V] {
  val vec = algebra.Vector(Nat(0))
  override type Type[A] = vec.Type[A]

  override implicit val locallyConcrete = vec

  def apply[F](input: V[F])(model: Type[F])(implicit field: Analytic[F]): V[F] =
    c.pointwise(f)(input)

  override def fill[F](x: => F)(implicit a: Additive[F]): Type[F] =
    vec.point(x)
}
