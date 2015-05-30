package cml.models

import cml._
import cml.algebra.traits._

case class LinAffinMap[In1[_], In2[_], Out[_]] (
  implicit in1Space: Concrete[In1],
  in2Space: Concrete[In2],
  outSpace: Concrete[Out]
) extends Model[({type T[A] = (In1[A], In2[A])})#T, Out] {
  implicit val affineMap = AffineMap[In2, Out]()(in2Space, outSpace)
  implicit val linAffinMap = LinearMap[In1, affineMap.Type]()(in1Space, affineMap.space)

  override type Type[A] = LinearMap[In1, affineMap.Type]#Type[A]
  override implicit val space = linAffinMap.space

  override def apply[F](inst: Type[F])(input: (In1[F], In2[F]))(implicit an: Analytic[F]): Out[F] =
    affineMap(linAffinMap(inst)(input._1))(input._2)
}
