package cml.models

import cml._
import cml.algebra.Representable.HashMapWithDefault
import cml.algebra.Subspace.Compose
import cml.algebra._

final case class HashMap[K, V[_]] (implicit
  valueSpace: Cartesian[V]
) extends Model[({type T[A] = K})#T, V] {
  override type Type[A] = HashMapWithDefault[K, V[A]]

  implicit val mapSpace = Representable.hashMap[K]
  override implicit val space =
    Representable.compose[({type T[A] = HashMapWithDefault[K, A]})#T, V](mapSpace, valueSpace)

  import ZeroEndofunctor.asZero

  def apply[A](inst: Type[A])(input: K)(implicit field: Analytic[A]): V[A] =
    mapSpace.index(inst)(input)

  override def applySubspace[A](
    subspace: Compose[({type T[A] = HashMapWithDefault[K, A]})#T, V, space.f.AllowedSubspace, space.g.AllowedSubspace],
    inst: Any)(input: K)(implicit a: Analytic[A]): V[A] = {
    // subspace.f: space.f.AllowedSubspace
    // space.f == mapSpace
    val f = subspace.f.asInstanceOf[mapSpace.AllowedSubspace]

    // subspace.g: space.g.AllowedSubspace
    // space.g == valueSpace
    val g = subspace.g.asInstanceOf[valueSpace.AllowedSubspace]

    // inst: subspace.Type[A]
    // subspace.Type[A] == subspace.f.Type[subspace.g.Type[A]]
    val i = inst.asInstanceOf[f.Type[g.Type[A]]]

    f.keyMap.get(input) match {
      case Some(k) => f.space.index(i)(k)
      case None => valueSpace.zero
    }
  }
}
