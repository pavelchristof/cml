package cml.models

import cml.Model
import cml.algebra._

case class LinearMap[In[_], Out[_]] (
  implicit
  inSpace: Cartesian[In],
  outSpace: Cartesian[Out]
) extends Model[In, Out] {
  override type Type[A] = Out[In[A]]

  override implicit val space: Cartesian[Type] = implicitly

  import ZeroFunctor.asZero

  override def apply[F](inst: Type[F])(input: In[F])(implicit a: Analytic[F]): Out[F] =
    outSpace.map(inst)(inSpace.dot(input, _))
}
