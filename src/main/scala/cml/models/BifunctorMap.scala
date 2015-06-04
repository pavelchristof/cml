package cml.models

import cml.Model
import cml.algebra._

import scalaz.Bifunctor

final case class BifunctorMap[F[_, _], A1[_], A2[_], B1[_], B2[_]] (
  left: Model[A1, A2],
  right: Model[B1, B2]
) (implicit
  f: T forSome {type T <: Bifunctor[F] with Serializable}
) extends Model[({type T[A] = F[A1[A], B1[A]]})#T, ({type T[A] = F[A2[A], B2[A]]})#T] {
  override type Type[A] = (left.Type[A], right.Type[A])

  override implicit val space = Representable.product(left.space, right.space)

  override def apply[A](inst: Type[A])(input: F[A1[A], B1[A]])(implicit a: Analytic[A]): F[A2[A], B2[A]] =
    f.bimap(input)(left(inst._1)(_), right(inst._2)(_))

  override def applySubspace[A](subspace: space.AllowedSubspace, instUntyped: Any)(input: F[A1[A], B1[A]])
      (implicit a: Analytic[A]): F[A2[A], B2[A]] = {
    val inst = instUntyped.asInstanceOf[subspace.Type[A]]
    val leftSubsp = subspace._1.asInstanceOf[left.space.AllowedSubspace]
    val rightSubsp = subspace._2.asInstanceOf[right.space.AllowedSubspace]
    f.bimap(input)(left.applySubspace(leftSubsp, inst._1)(_), right.applySubspace(rightSubsp, inst._2)(_))
  }
}
