package cml.models

import cml._
import cml.algebra._

final case class Duplicate[V[_]] () extends Model[V, ({type T[A] = (V[A], V[A])})#T] {
  override type Type[A] = Unit

  override implicit val space = Cartesian.Zero

  override def apply[F](inst: Type[F])(input: V[F])(implicit f: Analytic[F]): (V[F], V[F]) =
    (input, input)

  override def applySubspace[A](subspace: space.AllowedSubspace, inst: Any)(input: V[A])
      (implicit a: Analytic[A]): (V[A], V[A]) =
    (input, input)
}
