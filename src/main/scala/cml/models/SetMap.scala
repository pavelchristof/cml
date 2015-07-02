package cml.models

import cml._
import cml.algebra._

final case class SetMap[K, V[_]] (implicit
  valueSpace: Representable[V],
  ord: Ordering[K]
) extends Model[({type T[A] = K})#T, V] {
  override type Params[A] = TotalMap[K, V[A]]

  implicit val mapSpace = TotalMap.representable[K](ord)
  override implicit val space =
    Representable.compose[({type T[A] = TotalMap[K, A]})#T, V](mapSpace, valueSpace)

  import ClassTag1.asClassTag

  def apply[A](inst: Params[A])(input: K)(implicit field: Analytic[A]): V[A] =
    mapSpace.index(inst)(input)
}
