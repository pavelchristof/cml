package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class Pointwise[V[_]] (
  f: AnalyticMap
) (implicit
  c: Cartesian[V]
) extends Model[V, V] {
  override type Type[A] = Unit

  override implicit val space = Cartesian.Zero

  def apply[A](input: V[A])(implicit a: Analytic[A]): V[A] =
    c.map(input)(f(_))

  def apply[A](inst: Unit)(input: V[A])(implicit a: Analytic[A]): V[A] =
    apply(input)

  override def applySubspace[A](subspace: WholeSpace[Type], inst: Any)(input: V[A])
      (implicit a: Analytic[A]): V[A] =
    apply(input)
}
