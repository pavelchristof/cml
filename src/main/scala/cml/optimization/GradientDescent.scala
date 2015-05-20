package cml.optimization

import cml._
import cml.ad
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
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: cml.ad.Engine
  ): Vector[model.Type[A]] = {
    import diffEngine._
    import model.space

    // Select or create a model instance.
    val selector = SelectBest(model)
    var inst: model.Type[A] = selector(population.asInstanceOf[Vector[selector.model.Type[A]]], data, costFun)
      .applyOrElse(0, (_: Int) => model.symmetryBreaking(new Random())(fl))
      .asInstanceOf[model.Type[A]]

    def error(inst: model.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      implicit val an = analytic(fl, ctx)
      val scored = model.score(inst)(data.map(x => (
        inFunctor.map(x._1)(constant(_)),
        outFunctor.map(x._2)(constant(_)))
      ))
      costFun[model.Type, Aug[A]](inst, scored)
    }

    val gradWithErr = gradWithValueLC[A, model.Type](error(_, _))(fl, space)

    for (i <- 0 until iterations) {
      val (err, grad) = gradWithErr(inst)
      val gradStable = model.space.mapLC(grad)(x => if (fl.isNaN(x)) fl.zero else x)
      println(s"Iteration $i: $err")
      inst = space.sub(inst, space.mull(fl.fromDouble(step), gradStable))
    }

    Vector(inst)
  }
}
