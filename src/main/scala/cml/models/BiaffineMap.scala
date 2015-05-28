package cml.models

import cml._
import cml.algebra.traits._

case class BiaffineMap[In1[_], In2[_], Out[_]] (
  implicit in1Space: Concrete[In1],
  in2Space: Concrete[In2],
  outSpace: Concrete[Out]
) extends Model[algebra.Product[In1, In2]#Type, Out] {
  implicit val affineMap = AffineMap[In2, Out]()(in2Space, outSpace)
  implicit val biaffineMap = AffineMap[In1, affineMap.Type]()(in1Space, affineMap.space)

  override type Type[A] = AffineMap[In1, affineMap.Type]#Type[A]
  override implicit val space = biaffineMap.space

  override def apply[F](inst: Type[F])(input: (In1[F], In2[F]))(implicit an: Analytic[F]): Out[F] =
    affineMap(biaffineMap(inst)(input._1))(input._2)

  override def fill[F](x: => F)(implicit a: Additive[F]): Type[F] =
    biaffineMap.fill[F](x)
}
