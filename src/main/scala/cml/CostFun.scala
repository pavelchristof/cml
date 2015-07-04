package cml

import cml.algebra._
import org.apache.spark.rdd.RDD
import scala.collection.parallel.ParSeq

abstract class CostFun[In[_], Out[_]] extends Serializable {
  def scoreSample[A](sample: Sample[In[A], Out[A]])(implicit a: Analytic[A]): A

  def regularization[V[_], A](instance: V[A])(implicit a: Analytic[A], space: Normed[V]): A =
    a.zero

  def mean[A](data: RDD[Sample[In[A], Out[A]]])(implicit a: Analytic[A]): A = {
    import a.analyticSyntax._
    data.cache()
    data.map(scoreSample(_)).reduce(_ + _) / fromLong(data.count())
  }

  def sum[A](data: RDD[Sample[In[A], Out[A]]])(implicit a: Analytic[A]): A = {
    import a.analyticSyntax._
    data.map(scoreSample(_)).reduce(_ + _)
  }
}
