package cml.models

import cml._
import cml.algebra.Constant
import cml.algebra.traits._

case class Function[K, V[_]] (
  implicit e: Enumerate[K],
  valueSpace: Concrete[V]
  ) extends Model[({type T[A] = Constant[K, A]})#T, V] {
  override type Type[A] = Map[K, V[A]]

  val mapSpace = algebra.Map.locallyConcrete[K](e)
  override implicit val space =
    algebra.Compose[({type T[A] = Map[K, A]})#T, V].locallyConcrete(mapSpace, valueSpace)

  def apply[A](inst: Type[A])(input: Constant[K, A])(implicit field: Analytic[A]): V[A] =
    mapSpace.indexLC[V[A]](inst)(input.value)(valueSpace.additive)

  override def fill[A](x: => A)(implicit a: Additive[A]): Type[A] =
    Map().withDefault(_ => valueSpace.point(x))
}
