package cml

import cml.algebra._
import org.apache.spark.rdd.RDD

trait Optimizer[In[_], Out[_]] {
  val model: Model[In, Out]
  def apply[A](
    batches: RDD[Seq[(In[A], Out[A])]],
    costFun: CostFun[In, Out],
    initialInst: model.Type[A]
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): model.Type[A]
}
