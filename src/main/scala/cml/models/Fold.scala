package cml.models

import cml.{ParameterlessModel, Model}
import cml.algebra.{Analytic, Monoid1}

import scalaz.Foldable

final case class Fold[F[_], V[_]] (implicit
  foldable: Foldable[F],
  monoid: Monoid1[V]
) extends ParameterlessModel[({type T[A] = F[V[A]]})#T, V] {
  import Monoid1.asMonoid
  def apply[A](in: F[V[A]])(implicit a: Analytic[A]): V[A] = foldable.fold(in)
}
