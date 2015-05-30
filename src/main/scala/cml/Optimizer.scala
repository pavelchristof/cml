package cml

import cml.algebra._

trait Optimizer[In[_], Out[_]] {
  val model: Model[In, Out]
  def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    subspace: Subspace[model.Type],
    costFun: CostFun[In, Out],
    noise: => A
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): Vector[(A, model.Type[A])]
}
