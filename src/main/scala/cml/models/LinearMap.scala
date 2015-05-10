package cml.models

import cml.algebra
import cml.algebra.traits._

import scalaz.{Traverse, Applicative}

case class LinearMap[In[_] <: Serializable, Out[_] <: Serializable] (
  implicit outApplicative: Applicative[Out],
  inNormed: Normed[In],
  inTraverse: Traverse[In],
  outTraverse: Traverse[Out]
) extends Model[In, Out] {
  override type Type[A] = Out[In[A]]

  override implicit val traverse: Traverse[Type] = algebra.Compose.traverse
  override implicit val linear: Linear[Type] = algebra.Compose.linear

  override def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F] =
    outApplicative.map(model)(inNormed.dot(input, _))
}
