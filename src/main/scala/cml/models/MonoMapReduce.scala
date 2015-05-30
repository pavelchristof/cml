package cml.models

import cml._
import cml.algebra._

import scalaz.Const

trait MonoFoldable1[F, A] {
  def foldMap1[S](v: F)(inj: (A) => S, op: (S, S) => S): S
}

case class MonoMapReduce[F, E, V[_]] (
  map: Model[({type T[A] = Const[E, A]})#T, V],
  reduce: Model[({type T[A] = (V[A], V[A])})#T, V]
) (
  implicit monoFoldable: MonoFoldable1[F, E]
) extends Model[({type T[A] = Const[F, A]})#T, V] {
  type Type[A] = (map.Type[A], reduce.Type[A])

  override implicit val space = Representable.product(map.space, reduce.space)

  override def apply[A](inst: Type[A])(input: Const[F, A])(implicit a: Analytic[A]): V[A] =
    monoFoldable.foldMap1[V[A]](input.getConst)(
      x => map[A](inst._1)(Const(x)),
      reduce[A](inst._2)(_, _))
}
