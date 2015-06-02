package cml

import cml.algebra._
import scala.collection.parallel.ParSeq

abstract class CostFun[In[_], Out[_]] extends Serializable {
  def scoreSample[A](sample: Sample[In[A], Out[A]])(implicit a: Analytic[A]): A

  def regularization[V[_], A](instance: V[A])(implicit a: Analytic[A], space: Normed[V]): A

  def mean[A](data: ParSeq[Sample[In[A], Out[A]]])(implicit a: Analytic[A]): A = {
    import a.analyticSyntax._
    data.map(scoreSample(_)).reduce(_ + _) / fromLong(data.size)
  }

  def sum[A](data: ParSeq[Sample[In[A], Out[A]]])(implicit a: Analytic[A]): A = {
    import a.analyticSyntax._
    data.map(scoreSample(_)).reduce(_ + _)
  }
}
