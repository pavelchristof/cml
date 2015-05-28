package cml.models

import cml.Model
import cml.algebra.Compose
import cml.algebra.traits._

case class LinearMap[In[_], Out[_]] (
  implicit inLC: Concrete[In],
  outLC: Concrete[Out]
) extends Model[In, Out] {
  override type Type[A] = Out[In[A]]

  override implicit val space = Compose[Out, In].concrete(outLC, inLC)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] =
    outLC.map(inst)(inLC.dot(input, _))

  override def fill[F](x: => F)(implicit a: Additive[F]): Type[F] =
    outLC.point(inLC.point(x))
}
