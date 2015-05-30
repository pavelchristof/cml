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
    import subspace._

    def costOnSamples(samples: Seq[(In[A], Out[A])])(inst: Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      implicit val an = analytic(fl, ctx)
      samples.map(sample => {
        val input = inSpace.map(sample._1)(constant(_))
        val output = outSpace.map(sample._2)(constant(_))
        val scored = Sample(
          input = input,
          expected = output,
          actual = model(inject(inst))(input)
        )
        costFun.scoreSample(scored)
      }).fold(an.zero)(an.add(_, _))
    }

    def reg(inst: Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      costFun.regularization[Type, Aug[A]](inst)(analytic(fl, ctx), space)
    }

    def totalCost(inst: Type[A]): A =
      costFun.mean(model.applyParSeq(inject(inst))(data.par)) +
        costFun.regularization[Type, A](inst)(fl, space)

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

    var inst = project(instLC)
    var best = inst
    var bestCost = totalCost(best)
    val tr = gradTrans.create[Type, A]()(fl, space)

    for (i <- 1 to iterations) {
      var gradAcc = data
        .grouped(chunkSize)
        .map(samples =>
          grad[A, Type](costOnSamples(samples)(_, _))(fl, space)(inst)
        )
        .reduce(space.add(_, _))
      gradAcc = space.div(gradAcc, fl.fromInt(data.size))

      val gradReg = grad[A, Type](reg(_, _))(fl, space)(inst)
      gradAcc = space.add(gradAcc, gradReg)

      inst = space.sub(inst, tr(gradAcc))
      val cost = totalCost(inst)
      println(s"Iteration $i: ${cost}")

      if (cmp.lt(cost, bestCost)) {
        best = inst
        bestCost = cost
      }
    }

    Vector((bestCost, inject(best)))
  }
}
