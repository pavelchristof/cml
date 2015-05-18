package cml.models

import cml._
import cml.algebra.traits._

case class Constant[A, B] (value: A)

case class Function[K, V[_]] (
  implicit e: Enumerate[K],
  vlc: LocallyConcrete[V]
  ) extends Model[({type T[A] = Constant[K, A]}), V] {
  override type Type[A] = Map[K, V[A]]

  override implicit val space: LocallyConcrete[Type] =
    algebra.Compose().locallyConcrete(
      algebra.Map.locallyConcrete[K](e),
      vlc
    )

  def apply[A](inst: Type[A])(input: Constant[K, A])(implicit field: Analytic[A]): V[A] =
    inst(input.value)

  override def fill[A](x: => A)(implicit a: Additive[A]): Type[A] =
    Map()
}
