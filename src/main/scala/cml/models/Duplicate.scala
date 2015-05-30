package cml.models

import cml._
import cml.algebra.Analytic
import cml.algebra.traits.Analytic
import shapeless.Nat

case class Duplicate[V[_]] () extends Model[V, algebra.Product[V, V]#Type] {
  val vec = algebra.Vec(Nat(0))
  override type Type[A] = vec.Type[A]

  override implicit val space = vec

  override def apply[F](inst: Type[F])(input: V[F])(implicit f: Analytic[F]): (V[F], V[F]) = (input, input)
}
