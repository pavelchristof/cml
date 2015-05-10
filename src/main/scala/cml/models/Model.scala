package cml.models

import cml.algebra.Boolean._
import cml.algebra.traits._

import scalaz.{Traverse}

trait Model[-In[_], +Out[_]] {
  type Type[_]

  implicit val linear: Linear[Type]
  implicit val traverse: Traverse[Type]

  def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F]
  def fill[F](x: => F): Type[F] = traverse.map(linear.zero[Boolean])(_ => x)
}
