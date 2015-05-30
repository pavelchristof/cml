package cml.optimization

import cml._
import cml.algebra._

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
    noise: => A,
    subspace: Subspace[model.Type] = model.space.restrict(Set())
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: cml.ad.Engine
  ): Vector[(A, model.Type[A])] = {
    import fl.analyticSyntax._

    def score(inst: model.Type[A]): A =
      costFun.mean(model.applyParSeq(inst)(data.par)) +
        costFun.regularization[subspace.Type, A](subspace.project[A](inst))(fl, subspace.space)

    var p = population
      .par
      .map((inst: model.Type[A]) => (score(inst), inst))
      .filter(x => !fl.isNaN(x._1))
      .toVector
      .sortBy(_._1)
      .take(count)

    while (p.size < count) {
      val inst: model.Type[A] = subspace.inject(subspace.space.point(noise))
      val cost: A = score(inst)
      if (!fl.isNaN(cost)) {
        p +:= (cost, inst)
      }
    }

    p
  }
}
