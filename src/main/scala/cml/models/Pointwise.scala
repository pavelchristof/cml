package cml.models

import cml._
import cml.algebra._

case class Pointwise[V[_]] (
  f: AnalyticMap
) (implicit
  c: Cartesian[V]
) extends Model[V, V] {
  override type Type[A] = Unit

  override implicit val space = Cartesian.Zero

  def apply[A](inst: Type[A])(input: V[A])(implicit A: Analytic[A]): V[A] =
    c.map(input)(f(_))
}
