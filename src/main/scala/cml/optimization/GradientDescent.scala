package cml.optimization

import cml._
import cml.algebra._

/**
 * Basic gradient descent.
 */
case class GradientDescent[In[_], Out[_]] (
  model: Model[In, Out],
  iterations: Int,
  gradTrans: GradTrans = Stabilize,
  chunkSize: Int = 50
)(implicit
  inSpace: Representable[In],
  outSpace: Representable[Out]
) extends Optimizer[In, Out] {
  implicit val space = model.space

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
    import diffEngine._
    import fl.analyticSyntax._

    def costOnSamples(samples: Seq[(In[A], Out[A])])(inst: subspace.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      implicit val an = analytic(fl, ctx)
      samples.map(sample => {
        val input = inSpace.map(sample._1)(constant(_))
        val output = outSpace.map(sample._2)(constant(_))
        val scored = Sample(
          input = input,
          expected = output,
          actual = model[Aug[A]](subspace.inject(inst))(input)
        )
        costFun.scoreSample(scored)
      }).fold(an.zero)(an.add(_, _))
    }

    def reg(inst: subspace.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      costFun.regularization[subspace.Type, Aug[A]](inst)(analytic(fl, ctx), subspace.space)
    }

    def totalCost(inst: subspace.Type[A]): A =
      costFun.mean(model.applyParSeq(subspace.inject(inst))(data.par)) +
        costFun.regularization[subspace.Type, A](inst)(fl, subspace.space)

    // Select or create a model instance.
    val selector = SelectBest(model, count = 1)
    val instLC =
      selector(
        population.asInstanceOf[Vector[selector.model.Type[A]]],
        data,
        costFun,
        noise,
        subspace.asInstanceOf[Subspace[selector.model.Type]])
      .head
      ._2
      .asInstanceOf[model.Type[A]]

    var inst = subspace.project(instLC)
    var best = inst
    var bestCost = totalCost(best)
    val tr = gradTrans.create[subspace.Type, A]()(fl, subspace.space)

    for (i <- 1 to iterations) {
      var gradAcc = data
        .grouped(chunkSize)
        .map(samples =>
          grad[A, subspace.Type](costOnSamples(samples)(_, _))(fl, subspace.space)(inst)
        )
        .reduce(subspace.space.add(_, _))
      gradAcc = subspace.space.div(gradAcc, fl.fromInt(data.size))

      val gradReg = grad[A, subspace.Type](reg(_, _))(fl, subspace.space)(inst)
      gradAcc = subspace.space.add(gradAcc, gradReg)

      inst = subspace.space.sub(inst, tr(gradAcc))
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
