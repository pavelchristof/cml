package cml

import cml.algebra._

trait Optimizer {
  def apply[In[_], Out[_], A] (
    model: Model[In, Out]
  ) (
    batches: Vector[Seq[(In[A], Out[A])]],
    costFun: CostFun[In, Out],
    initialInst: model.Params[A]
  ) (implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine,
    in: Functor[In],
    out: Functor[Out]
  ): model.Params[A]
}
