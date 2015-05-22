package cml.optimization

import cml._
import cml.algebra.traits.Floating

case class MultiOpt[In[_], Out[_]] (
  populationSize: Int,
  optimizer: Optimizer[In, Out]
) extends Optimizer[In, Out] {
  override final val model = optimizer.model

  def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    costFun: CostFun[In, Out],
    default: => A
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): Vector[(A, model.Type[A])] = {
    val selector = SelectBest(model, populationSize)
    val instances: Vector[model.Type[A]] =
      selector(
        population.asInstanceOf[Vector[selector.model.Type[A]]],
        data,
        costFun,
        default)
      .map(_._2)
      .asInstanceOf[Vector[model.Type[A]]]

    instances
      .toParArray
      .flatMap(inst => optimizer(
        Vector(inst.asInstanceOf[optimizer.model.Type[A]]),
        data,
        costFun,
        default
      ))
      .toVector
      .asInstanceOf[Vector[(A, model.Type[A])]]
  }
}
