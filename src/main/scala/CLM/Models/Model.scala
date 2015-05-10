package CLM.Models

import CML.Algebra.{Linear, Analytic}
import CML.Algebra.Boolean._

import scalaz.Functor

trait Model[M[_] <: Serializable] extends Linear[M] with Functor[M] {
  type In[_]
  type Out[_]
  type Type[A] = M[A]

  def predict[F](input: In[F], model: Type[F])(implicit f: Analytic[F]): Out[F]
  def fill[F](x: => F): Type[F] = map(zero[Boolean])(_ => x)
}
