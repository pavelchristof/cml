package cml.models

import cml._
import cml.algebra._

import scalaz.Functor

case class FunctorMap[F[_], G[_], H[_]](
  map: Model[G, H]
) (implicit
  functor: Functor[F]
) extends Model[({type T[A] = F[G[A]]})#T, ({type T[A] = F[H[A]]})#T] {
  override type Type[A] = map.Type[A]

  override implicit val space = map.space

  override def apply[A](inst: Type[A])(input: F[G[A]])(implicit field: Analytic[A]): F[H[A]] =
    functor.map(input)(map(inst)(_))
}
