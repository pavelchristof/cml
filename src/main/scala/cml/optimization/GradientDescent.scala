package cml.optimization

import cml._
import cml.algebra.ad
import cml.algebra.traits._

import scala.util.Random
import scalaz.Functor

/**
 * Basic gradient descent.
 */
case class GradientDescent[In[_], Out[_]] (
  model: Model[In, Out],
  iterations: Int,
  step: Double
)(implicit
  inFunctor: Functor[In],
  outFunctor: Functor[Out]
) extends Optimizer[In, Out] {
  override def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    costFun: CostFun[In, Out]
  )(implicit
    an: Analytic[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): Vector[model.Type[A]] = {
    import diffEngine._
    import model.space

    // Select or create a model instance.
    val selector = SelectBest(model)
    var inst: model.Type[A] = selector(population.asInstanceOf[Vector[selector.model.Type[A]]], data, costFun)
      .applyOrElse(0, (_: Int) => model.symmetryBreaking(new Random())(an))
      .asInstanceOf[model.Type[A]]

    def error(inst: model.Type[Aug[A]]): Aug[A] = {
      val scored = model.score(inst)(data.map(x => (
        inFunctor.map(x._1)(constant(_)),
        outFunctor.map(x._2)(constant(_)))
      ))
      costFun[model.Type, Aug[A]](inst, scored)
    }

    val gradWithErr = gradWithValueLC[A, model.Type](error(_))(an, space)

    for (i <- 0 until iterations) {
      val (err, grad) = gradWithErr(inst)
      println(s"Iteration $i: $err")
      inst = space.sub(inst, space.mull(an.fromDouble(step), grad))
    }

    Vector(inst)
  }
}
