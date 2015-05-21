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
  ): Vector[model.Type[A]] = {
    val p = population
      .map((inst: model.Type[A]) =>
        (inst, costFun[model.Type, A](inst, model.score(inst)(data))(fl, model.space)))
      .filter(x => !fl.isNaN(x._2))
    if (p.isEmpty) {
      Vector()
    } else {
      Vector(p.minBy(_._2)._1)
    }
  }
}
