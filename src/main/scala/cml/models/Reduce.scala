package cml.models

import cml.Model
import cml.algebra.Compose
import cml.algebra.traits.{Additive, Analytic}

import scalaz.{Foldable1, Semigroup}

case class Reduce[-F[_], R[_]] (
  m: Model[({type T[A] = (R[A], R[A])})#T, R]
) (
  implicit foldable: Foldable1[F]
) extends Model[Compose[F, R]#Type, R] {
  type Type[A] = m.Type[A]

  override implicit val locallyConcrete = m.locallyConcrete

  override def apply[A](input: F[R[A]])(model: Type[A])(implicit field: Analytic[A]): R[A] =
    foldable.fold1(input)(new Semigroup[R[A]] {
      override def append(f1: R[A], f2: => R[A]): R[A] = m((f1, f2))(model)
    })

  def fill[A](x: => A)(implicit a: Additive[A]): Type[A] = m.fill(x)
}
