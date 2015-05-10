package cml.models

import cml.algebra.traits._

trait Model[-In[_], +Out[_]] {
  type Type[_] <: Serializable

  implicit val concrete: Concrete[Type]
  val normed: Normed[Type] = concrete
  val linear: Linear[Type] = normed
  val additive1: Additive1[Type] = linear

  def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F]
  def fill[F](x: => F)(implicit f: Field[F]): Type[F] = concrete.tabulate(_ => x)
}
