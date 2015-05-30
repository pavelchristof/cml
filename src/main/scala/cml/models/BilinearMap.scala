package cml.models

import cml._
import cml.algebra.traits._

case class BilinearMap[In1[_], In2[_], Out[_]] (
  implicit in1Space: Concrete[In1],
  in2Space: Concrete[In2],
  outSpace: Concrete[Out]
) extends Model[algebra.Product[In1, In2]#Type, Out] {
  implicit val linearMap = LinearMap[In2, Out]()(in2Space, outSpace)
  implicit val bilinearMap = LinearMap[In1, linearMap.Type]()(in1Space, linearMap.space)

  override type Type[A] = LinearMap[In1, linearMap.Type]#Type[A]
  override implicit val space = bilinearMap.space

  override def apply[F](inst: Type[F])(input: (In1[F], In2[F]))(implicit an: Analytic[F]): Out[F] =
    linearMap(bilinearMap(inst)(input._1))(input._2)
}
