package cml.models

import cml.algebra.Compose
import cml.algebra.traits._

case class LinearMap[In[_] <: Serializable, Out[_] <: Serializable] (
  implicit inConcrete: Concrete[In],
  outConcrete: Concrete[Out]
) extends Model[In, Out] {
  override type Type[A] = Out[In[A]]

  override implicit val concrete = new Compose.ComposeConcrete[Out, In]()

  override def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F] =
    outConcrete.map(model)(inConcrete.dot(input, _))
}
