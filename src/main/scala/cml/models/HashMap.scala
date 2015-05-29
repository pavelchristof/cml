package cml.models

import cml._
import cml.algebra.traits._

import scalaz.Const

case class HashMap[K, V[_]] (
  implicit e: Enumerate[K],
  valueSpace: Concrete[V]
  ) extends Model[({type T[A] = Const[K, A]})#T, V] {
  override type Type[A] = collection.immutable.HashMap[K, V[A]]

  val mapSpace = algebra.HashMapInstances.locallyConcrete[K](e)
  override implicit val space =
    algebra.Compose[({type T[A] = collection.immutable.HashMap[K, A]})#T, V].locallyConcrete(mapSpace, valueSpace)

  def apply[A](inst: Type[A])(input: Const[K, A])(implicit field: Analytic[A]): V[A] =
    mapSpace.indexLC[V[A]](inst)(input.getConst)(valueSpace.additive)

  override def fill[A](x: => A)(implicit a: Additive[A]): Type[A] =
    collection.immutable.HashMap()
}
