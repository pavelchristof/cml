package cml.optimization

import cml._
import cml.algebra._

case class MultiOpt[In[_], Out[_]] (
  populationSize: Int,
  optimizer: Optimizer[In, Out]
) extends Optimizer[In, Out] {
  override final val model = optimizer.model

  def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    costFun: CostFun[In, Out],
    noise: => A,
    subspace: Subspace[model.Type] = model.space.restrict(Set())
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): Vector[(A, model.Type[A])] = {
    val selector = SelectBest(model, populationSize)
    val instances =
      selector(
        population.asInstanceOf[Vector[selector.model.Type[A]]],
        data,
        costFun,
        noise,
        subspace.asInstanceOf[Subspace[selector.model.Type]])
      .map(_._2)
      .asInstanceOf[Vector[model.Type[A]]]

    instances
      .par
      .flatMap(inst => optimizer(
        Vector(inst.asInstanceOf[optimizer.model.Type[A]]),
        data,
        costFun,
        noise
      ))
      .toVector
      .asInstanceOf[Vector[(A, model.Type[A])]]
  }
}
