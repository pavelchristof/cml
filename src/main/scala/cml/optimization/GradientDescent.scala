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
  gradTrans: GradTrans = Stabilize
)(implicit
  inFunctor: Functor[In],
  outFunctor: Functor[Out]
) extends Optimizer[In, Out] {
  implicit val space = model.space

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
    import diffEngine._
    import fl.analyticSyntax._

    // Select or create a model instance.
    val selector = SelectBest(model, count = 1)
    var inst: model.Type[A] =
      selector(
        population.asInstanceOf[Vector[selector.model.Type[A]]],
        data,
        costFun,
        default)
      .head
      ._2
      .asInstanceOf[model.Type[A]]

    def costOnSample(sample: (In[A], Out[A]))(inst: model.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      implicit val an = analytic(fl, ctx)
      val input = inFunctor.map(sample._1)(constant(_))
      val output = outFunctor.map(sample._2)(constant(_))
      val scored = Sample(
        input = input,
        expected = output,
        actual = model(inst)(input)
      )
      costFun.scoreSample(scored)
    }

    def reg(inst: model.Type[Aug[A]], ctx:Context[A]): Aug[A] = {
      costFun.regularization[model.Type, Aug[A]](inst)(analytic(fl, ctx), space)
    }

    def totalCost(inst: model.Type[A]): A =
      costFun.mean(model.score(inst)(data)) + costFun.regularization[model.Type, A](inst)

    val tr = gradTrans.create()
    var best = inst
    var bestCost = totalCost(best)

    for (i <- 1 to iterations) {
      var gradAcc = data
        .toParArray
        .map(sample => {
          gradLC[A, model.Type](costOnSample(sample)(_, _))(fl, space)(inst)
        })
        .fold(space.zero)(space.add(_, _))
      gradAcc = space.div(gradAcc, fl.fromInt(data.size))

      val gradReg = gradLC[A, model.Type](reg(_, _))(fl, space)(inst)
      gradAcc = space.add(gradAcc, gradReg)

      inst = space.sub(inst, tr(gradAcc))
      val cost = totalCost(inst)
      println(s"Iteration $i: ${cost}")

      if (cmp.lt(cost, bestCost)) {
        best = inst
        bestCost = cost
      }
    }

    Vector((bestCost, best))
  }
}
