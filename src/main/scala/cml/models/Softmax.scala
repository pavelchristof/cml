package cml.models

import cml._
import cml.algebra._

case class Softmax[V[_]] (
  implicit vs: Normed[V]
) extends Model[V, V] {
  override type Type[A] = Unit
  override implicit val space = Cartesian.Zero

  def apply[A](inst: Type[A])(input: V[A])(implicit a: Analytic[A]): V[A] = {
    val expd = vs.map(input)(a.exp)
    val total = vs.sum(expd)
    vs.map(expd)(a.div(_, total))
  }
}
