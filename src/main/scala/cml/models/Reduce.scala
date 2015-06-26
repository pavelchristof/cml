package cml.models

import cml.Model
import cml.algebra._

import scalaz.{Foldable1, Semigroup}

final case class Reduce[F[_], R[_]] (
  m: Model[({type T[A] = (R[A], R[A])})#T, R]
) (
  implicit foldable: Foldable1[F]
) extends Model[({type T[A] = F[R[A]]})#T, R] {
  type Params[A] = m.Params[A]

  override implicit val space = m.space

  override def apply[A](inst: Params[A])(input: F[R[A]])(implicit a: Analytic[A]): R[A] =
    foldable.fold1(input)(new Semigroup[R[A]] {
      override def append(f1: R[A], f2: => R[A]): R[A] = m(inst)((f1, f2))
    })
}
