package cml.optimization

import cml._
import cml.ad
import cml.algebra.traits._

/**
 * Selects the best instance from the population.
 */
case class SelectBest[In[_], Out[_]] (
  model: Model[In, Out],
  count: Int
) extends Optimizer[In, Out] {
  override def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    costFun: CostFun[In, Out],
    default: => A
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: cml.ad.Engine
  ): Vector[(A, model.Type[A])] = {
    def score(inst: model.Type[A]): A =
      costFun[model.Type, A](inst, model.score(inst)(data))(fl, model.space)

    var p = population
      .map((inst: model.Type[A]) => (score(inst), inst))
      .filter(x => !fl.isNaN(x._1))
      .sortBy(_._1)
      .take(count)

    while (p.size < count) {
      val inst: model.Type[A] = model.fill(default)
      val cost: A = score(inst)
      if (!fl.isNaN(cost)) {
        p +:= (cost, model.fill(default))
      }
    }

    p
  }
}
