package cml

import cml.algebra._
import scala.collection.parallel.ParSeq

abstract class CostFun[In[_], Out[_]] extends Serializable {
  def scoreSample[A](sample: Sample[In[A], Out[A]])(implicit an: Analytic[A]): A

  def regularization[V[_], A](instance: V[A])(implicit an: Analytic[A], space: Concrete[V]): A

  def mean[A](data: ParSeq[Sample[In[A], Out[A]]])(implicit an: Analytic[A]): A = {
    import an.analyticSyntax._
    data.map(scoreSample(_)).reduce(_ + _) / fromLong(data.size)
  }

  def sum[A](data: ParSeq[Sample[In[A], Out[A]]])(implicit an: Analytic[A]): A = {
    import an.analyticSyntax._
    data.map(scoreSample(_)).reduce(_ + _)
  }
}
