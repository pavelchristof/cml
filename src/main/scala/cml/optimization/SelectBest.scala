package cml.optimization

import cml._
import cml.ad
import cml.algebra.traits._

/**
 * Selects the best instance from the population.
 */
case class SelectBest[In[_], Out[_]] (
  model: Model[In, Out]
) extends Optimizer[In, Out] {
  override def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    costFun: CostFun[In, Out]
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: cml.ad.Engine
  ): Vector[model.Type[A]] =
    if (population.isEmpty) {
      Vector.empty
    } else {
      Vector(population.minBy((inst: model.Type[A]) =>
        costFun[model.Type, A](inst, model.score(inst)(data))(fl, model.space)
      ))
    }
}
