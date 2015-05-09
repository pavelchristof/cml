package CLM.Models

import CML.Algebra.{Linear, Analytic}

trait Model[M[_] <: Serializable, In[_], Out[_]] extends Linear[M] {
  def predict[F](input: In[F], model: M[F])(implicit f: Analytic[F]): Out[F]
}
