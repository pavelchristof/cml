package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class BiaffineMap[In1[_], In2[_], Out[_]] (implicit
  in1Space: Cartesian[In1],
  in2Space: Cartesian[In2],
  outSpace: Cartesian[Out]
) extends Model[({type T[A] = (In1[A], In2[A])})#T, Out] {
  implicit val affineMap = AffineMap[In2, Out]()(in2Space, outSpace)
  implicit val biaffineMap = AffineMap[In1, affineMap.Type]()(in1Space, affineMap.space)

  override type Type[A] = AffineMap[In1, affineMap.Type]#Type[A]
  override implicit val space = biaffineMap.space

  override def apply[F](inst: Type[F])(input: (In1[F], In2[F]))(implicit an: Analytic[F]): Out[F] =
    affineMap(biaffineMap(inst)(input._1))(input._2)
}
