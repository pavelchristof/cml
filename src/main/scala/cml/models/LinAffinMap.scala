package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class LinAffinMap[In1[_], In2[_], Out[_]] (implicit
  in1Space: Cartesian[In1],
  in2Space: Cartesian[In2],
  outSpace: Cartesian[Out]
) extends Model[({type T[A] = (In1[A], In2[A])})#T, Out] {
  implicit val affineMap = AffineMap[In2, Out]()(in2Space, outSpace)
  implicit val linAffinMap = LinearMap[In1, affineMap.Type]()(in1Space, affineMap.space)

  override type Type[A] = LinearMap[In1, affineMap.Type]#Type[A]
  override implicit val space = linAffinMap.space

  override def apply[A](inst: Type[A])(input: (In1[A], In2[A]))(implicit a: Analytic[A]): Out[A] =
    affineMap(linAffinMap(inst)(input._1))(input._2)
}
