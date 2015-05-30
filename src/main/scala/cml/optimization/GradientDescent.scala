package cml.optimization

import cml._
import cml.algebra.traits._

import scalaz.Functor

/**
 * Basic gradient descent.
 */
case class GradientDescent[In[_], Out[_]] (
  model: Model[In, Out],
  iterations: Int,
  gradTrans: GradTrans = Stabilize,
  chunkSize: Int = 50
)(implicit
  inLC: LocallyConcrete[In],
  outLC: LocallyConcrete[Out]
) extends Optimizer[In, Out] {
  implicit val space = model.space

  override def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    subspace: Subspace[model.Type],
    costFun: CostFun[In, Out],
    noise: => A
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: cml.ad.Engine
  ): Vector[(A, model.Type[A])] = {
    import diffEngine._
    import fl.analyticSyntax._

    def costOnSamples(samples: Seq[(In[A], Out[A])])(inst: subspace.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      implicit val an = analytic(fl, ctx)
      samples.map(sample => {
        val input = inFunctor.map(sample._1)(constant(_))
        val output = outFunctor.map(sample._2)(constant(_))
        val scored = Sample(
          input = input,
          expected = output,
          actual = model[Aug[A]](subspace.inject(inst))(input)
        )
        costFun.scoreSample(scored)
      }).fold(an.zero)(an.add(_, _))
    }

    def reg(inst: subspace.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      costFun.regularization[subspace.Type, Aug[A]](inst)(analytic(fl, ctx), subspace.concrete)
    }

    def totalCost(inst: subspace.Type[A]): A =
      costFun.mean(model.applyParSeq(subspace.inject(inst))(data.par)) +
        costFun.regularization[subspace.Type, A](inst)(fl, subspace.concrete)

    // Select or create a model instance.
    val selector = SelectBest(model, count = 1)
    val instLC =
      selector(
        population.asInstanceOf[Vector[selector.model.Type[A]]],
        data,
        subspace.asInstanceOf[Subspace[selector.model.Type]],
        costFun,
        noise)
      .head
      ._2
      .asInstanceOf[model.Type[A]]

    var inst = subspace.project(instLC)
    var best = inst
    var bestCost = totalCost(best)
    val tr = gradTrans.create[subspace.Type, A]()(fl, subspace.concrete)

    for (i <- 1 to iterations) {
      var gradAcc = data
        .grouped(chunkSize)
        .map(samples =>
          grad[A, subspace.Type](costOnSamples(samples)(_, _))(fl, subspace.concrete)(inst)
        )
        .reduce(subspace.concrete.add(_, _))
      gradAcc = subspace.concrete.div(gradAcc, fl.fromInt(data.size))

      val gradReg = grad[A, subspace.Type](reg(_, _))(fl, subspace.concrete)(inst)
      gradAcc = subspace.concrete.add(gradAcc, gradReg)

      inst = subspace.concrete.sub(inst, tr(gradAcc))
      val cost = totalCost(inst)
      println(s"Iteration $i: ${cost}")

      if (cmp.lt(cost, bestCost)) {
        best = inst
        bestCost = cost
      }
    }

    Vector((bestCost, subspace.inject(best)))
  }
}
