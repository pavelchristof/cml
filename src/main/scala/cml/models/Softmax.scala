package cml.models

import cml.{Model, algebra}
import cml.algebra.traits._
import shapeless.Nat

case class Softmax[V[_]] (
  implicit lc: LocallyConcrete[V]
) extends Model[V, V] {
  val vec = algebra.Vector(Nat(0))
  override implicit val space = vec
  override type Type[A] = vec.Type[A]

  def apply[A](inst: Type[A])(input: V[A])(implicit field: Analytic[A]): V[A] = {
    val expd = lc.mapLC(input)(field.exp)
    val total = lc.sum(expd)
    lc.mapLC(expd)(field.div(_, total))
  }

  override def fill[A](x: => A)(implicit a: Additive[A]): Type[A] =
    space.point(x)
}
