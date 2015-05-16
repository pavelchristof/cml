package cml.models

import cml.Model
import cml.algebra.Compose
import cml.algebra.traits._

case class LinearMap[In[_], Out[_]] (
  implicit inLC: Concrete[In],
  outLC: Concrete[Out]
) extends Model[In, Out] {
  override type Type[A] = Out[In[A]]

  override implicit val space = new Compose.ComposeLocallyConcrete[Out, In]()

  override def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F] =
    outLC.map(model)(inLC.dot(input, _))

  override def fill[F](x: => F)(implicit a: Additive[F]): Type[F] =
    outLC.point(inLC.point(x))
}
