package cml

import cml.algebra.traits._

abstract class CostFun[In[_], Out[_]] {
  def scoreSample[A](sample: ScoredSample[In[A], Out[A]])(implicit an: Analytic[A]): A
  def regularization[V[_], A](instance: V[A])(implicit an: Analytic[A], space: LocallyConcrete[V]): A

  def mean[A](data: Seq[ScoredSample[In[A], Out[A]]])(implicit an: Analytic[A]): A = {
    import an.analyticSyntax._
    data.map(scoreSample(_)).fold[A](an.zero)(_ + _) / data.size
  }

  def apply[V[_], A](
    instance: V[A],
    data: Seq[ScoredSample[In[A], Out[A]]]
  )(implicit an: Analytic[A], space: LocallyConcrete[V]): A =
    an.add(mean(data), regularization(instance))
}
