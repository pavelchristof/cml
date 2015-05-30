package cml.models

import cml._
import cml.algebra.Compose
import cml.algebra.traits._

import scalaz.Functor

case class FunctorMap[F[_], G[_], H[_]](
  map: Model[G, H]
) (
  implicit functor: Functor[F]
) extends Model[Compose[F, G]#Type, Compose[F, H]#Type] {
  override type Type[A] = map.Type[A]

  override implicit val space: LocallyConcrete[Type] = map.space

  override def apply[A](inst: Type[A])(input: Compose[F, G]#Type[A])
    (implicit field: Analytic[A]): Compose[F, H]#Type[A] =
    functor.map(input)(map(inst)(_))
}
