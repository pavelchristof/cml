package cml.optimization

import cml._
import cml.algebra._
import org.apache.spark.rdd.RDD

/**
 * Basic gradient descent.
 */
case class GradientDescent[In[_], Out[_]] (
  model: Model[In, Out],
  iterations: Int,
  gradTrans: GradTrans = Stabilize,
  chunkSize: Int = 50
)(implicit
  inFunctor: ZeroFunctor[In],
  outFunctor: ZeroFunctor[Out]
) extends Optimizer[In, Out] {

  def apply[A](
    subspace: model.space.AllowedSubspace,
    data: RDD[(In[A], Out[A])],
    costFun: CostFun[In, Out],
    initialInst: Any // subspace.Type[A]
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): model.Type[A] = {
    import diffEngine._
    import fl.analyticSyntax._

    def costOnSamples(samples: Iterator[(In[A], Out[A])])(inst: subspace.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      implicit val an = analytic(fl, ctx)
      samples.map(sample => {
        val input = inFunctor.map(sample._1)(constant(_))
        val output = outFunctor.map(sample._2)(constant(_))
        val scored = Sample(
          input = input,
          expected = output,
          actual = model.applySubspace(subspace, inst)(input)
        )
        costFun.scoreSample(scored)
      }).fold(an.zero)(an.add(_, _))
    }

    def reg(inst: subspace.Type[Aug[A]], ctx: Context[A]): Aug[A] = {
      costFun.regularization[subspace.Type, Aug[A]](inst)(analytic(fl, ctx), subspace.space)
    }

    def totalCost(inst: subspace.Type[A]): A =
      costFun.mean(model.applySubspaceRDD(subspace, inst)(data)) +
        costFun.regularization[subspace.Type, A](inst)(fl, subspace.space)

    var inst = initialInst.asInstanceOf[subspace.Type[A]]
    var best = inst
    var bestCost = totalCost(best)
    val tr = gradTrans.create[subspace.Type, A]()(fl, subspace.space)

    data.cache()
    for (i <- 1 to iterations) {
      var gradAcc = data
        .mapPartitions(samples => Iterator(
          grad[A, subspace.Type](costOnSamples(samples)(_, _))(fl, subspace.space)(inst)
        ))(ZeroFunctor.asZero(subspace.space, fl))
        .reduce(subspace.space.add(_, _))
      gradAcc = subspace.space.div(gradAcc, fl.fromLong(data.count()))

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

    subspace.inject(best)
  }
}
