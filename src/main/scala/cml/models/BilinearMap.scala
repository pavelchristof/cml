package cml.models

import cml._
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class BilinearMap[In1[_], In2[_], Out[_]] (implicit
  in1Space: Cartesian[In1],
  in2Space: Cartesian[In2],
  outSpace: Cartesian[Out]
) extends Model[({type T[A] = (In1[A], In2[A])})#T, Out] {
  implicit val linearMap = LinearMap[In2, Out]()(in2Space, outSpace)
  implicit val bilinearMap = LinearMap[In1, linearMap.Type]()(in1Space, linearMap.space)

  override type Type[A] = LinearMap[In1, linearMap.Type]#Type[A]
  override implicit val space = bilinearMap.space

  override def apply[F](inst: Type[F])(input: (In1[F], In2[F]))(implicit a: Analytic[F]): Out[F] =
    linearMap(bilinearMap(inst)(input._1))(input._2)
}
