package cml.models

import cml.algebra.{Normed, Analytic}
import cml.{Model, algebra}
import cml.algebra.traits._
import shapeless.Nat

case class Softmax[V[_]] (
  implicit vs: Normed[V]
) extends Model[V, V] {
  val vec = algebra.Vec(Nat(0))
  override implicit val space = vec
  override type Type[A] = vec.Type[A]

  def apply[A](inst: Type[A])(input: V[A])(implicit field: Analytic[A]): V[A] = {
    val expd = vs.map(input)(field.exp)
    val total = vs.sum(expd)
    vs.map(expd)(field.div(_, total))
  }
}
