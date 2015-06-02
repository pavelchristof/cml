package cml

import cml.algebra._

trait Optimizer[In[_], Out[_]] {
  val model: Model[In, Out]
  def apply[A](
    subspace: model.space.AllowedSubspace,
    data: Seq[(In[A], Out[A])],
    costFun: CostFun[In, Out],
    initialInst: Any // subspace.Type[A]
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): model.Type[A]
}
