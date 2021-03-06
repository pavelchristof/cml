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
  type Params[A] = (map.Params[A], reduce.Params[A])

  override implicit val params = Representable.product(map.params, reduce.params)

  override def apply[A](inst: Params[A])(input: F)(implicit a: Analytic[A]): V[A] =
    monoFoldable.foldMap1[V[A]](input)(
      x => map[A](inst._1)(x),
      reduce[A](inst._2)(_, _))
}
