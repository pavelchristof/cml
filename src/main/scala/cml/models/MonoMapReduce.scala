package cml.models

import cml._
import cml.algebra.Subspace.Product
import cml.algebra._

trait MonoFoldable1[F, A] extends Serializable {
  def foldMap1[S](v: F)(inj: (A) => S, op: (S, S) => S): S
}

final case class MonoMapReduce[F, E, V[_]] (
  map: Model[({type T[A] = E})#T, V],
  reduce: Model[({type T[A] = (V[A], V[A])})#T, V]
) (
  implicit monoFoldable: MonoFoldable1[F, E]
) extends Model[({type T[A] = F})#T, V] {
  type Type[A] = (map.Type[A], reduce.Type[A])

  override implicit val space = Representable.product(map.space, reduce.space)

  override def apply[A](inst: Type[A])(input: F)(implicit a: Analytic[A]): V[A] =
    monoFoldable.foldMap1[V[A]](input)(
      x => map[A](inst._1)(x),
      reduce[A](inst._2)(_, _))

  override def applySubspace[A](
    subspace: Product[map.Type, reduce.Type, space.f.AllowedSubspace, space.g.AllowedSubspace], inst: Any)
      (input: F)(implicit a: Analytic[A]): V[A] = {
    val i = inst.asInstanceOf[subspace.Type[A]]

    // map.space == space.f
    val _1 = subspace._1.asInstanceOf[map.space.AllowedSubspace]

    // reduce.space == space.g
    val _2 = subspace._2.asInstanceOf[reduce.space.AllowedSubspace]

    monoFoldable.foldMap1[V[A]](input)(
      x => map.applySubspace[A](_1, i._1)(x),
      reduce.applySubspace[A](_2, i._2)(_, _))
  }
}
