package cml.models

import cml._
import cml.algebra.traits._

import scalaz.Const

trait MonoFoldable1[F, A] {
  def foldMap1[S](v: F)(inj: (A) => S, op: (S, S) => S): S
}

case class MonoMapReduce[F, E, V[_]] (
  map: Model[({type T[A] = Const[E, A]})#T, V],
  reduce: Model[algebra.Product[V, V]#Type, V]
) (
  implicit monoFoldable: MonoFoldable1[F, E]
) extends Model[({type T[A] = Const[F, A]})#T, V] {
  type Type[A] = (map.Type[A], reduce.Type[A])

  override implicit val space = algebra.Product.locallyConcrete[map.Type, reduce.Type](map.space, reduce.space)

  override def apply[A](inst: Type[A])(input: Const[F, A])(implicit field: Analytic[A]): V[A] =
    monoFoldable.foldMap1[V[A]](input.getConst)(
      x => map[A](inst._1)(Const(x)),
      reduce[A](inst._2)(_, _))

  def fill[A](x: => A)(implicit a: Additive[A]): Type[A] = (map.fill(x), reduce.fill(x))
}
