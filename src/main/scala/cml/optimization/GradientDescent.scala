package cml.optimization

import cml._
import cml.algebra._
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD

case class SparkGD[In[_], Out[_], A](
  modelBC: Broadcast[Model[In, Out]],
  costFunBC: Broadcast[CostFun[In, Out]],
  diffEngineBC: Broadcast[ad.Engine],
  flBC: Broadcast[Floating[A]]
) extends Serializable {
  @transient lazy val model = modelBC.value
  @transient lazy val costFun = costFunBC.value
  @transient lazy val diffEngine = diffEngineBC.value
  @transient lazy implicit val fl = flBC.value

  def gradSamples(
    subspaceBC: Broadcast[model.space.AllowedSubspace],
    data: RDD[(In[diffEngine.Aug[A]], Out[diffEngine.Aug[A]])],
    count: Long,
    instUntyped: Any // subspace.Type[A]
  ): Any = {
    val grad: Any = data
      .mapPartitions[Any](samples => {
        val subspace = subspaceBC.value
        val inst = instUntyped.asInstanceOf[subspace.Type[A]]

        val grad = diffEngine.grad[A, subspace.Type]((x, ctx) => {
          implicit val a = diffEngine.analytic(fl, ctx)
          samples
            .map(sample => Sample(
              input = sample._1,
              expected = sample._2,
              actual = model.applySubspace(subspace, x)(sample._1)
            ))
            .map(costFun.scoreSample(_))
            .fold(a.zero)(a.add(_, _))
        })(fl, subspace.space)(inst)

        Iterator(grad)
      })
      .reduce((a, b) => {
        val subspace = subspaceBC.value
        subspace.space.add[A](
          a.asInstanceOf[subspace.Type[A]],
          b.asInstanceOf[subspace.Type[A]]
        )
      })

    val subspace = subspaceBC.value
    subspace.space.div[A](
      grad.asInstanceOf[subspace.Type[A]],
      fl.fromLong(count)
    )
  }

  def gradReg(
    subspace: model.space.AllowedSubspace,
    instUntyped: Any // subspace.Type[A]
  ): Any = {
    val inst = instUntyped.asInstanceOf[subspace.Type[A]]
    diffEngine.grad[A, subspace.Type]((x, ctx) =>
      costFun.regularization(x)(diffEngine.analytic(fl, ctx), subspace.space))(fl, subspace.space)(inst)
  }

  def totalCost(
    subspaceBC: Broadcast[model.space.AllowedSubspace],
    data: RDD[(In[A], Out[A])],
    instUntyped: Any // subspace.Type[A]
  ): A = {
    val j = data
      .map(sample => Sample(
        input = sample._1,
        expected = sample._2,
        actual = model.applySubspace(subspaceBC.value, instUntyped)(sample._1)
      ))
      .map(costFun.scoreSample(_))
      .reduce(fl.add(_, _))

    val subspace = subspaceBC.value
    val inst = instUntyped.asInstanceOf[subspace.Type[A]]
    val r = costFun.regularization(inst)(fl, subspace.space)

    fl.add(j, r)
  }
}

/**
 * Basic gradient descent.
 */
case class GradientDescent[In[_], Out[_]] (
  model: Model[In, Out],
  iterations: Int,
  gradTrans: GradTrans = Stabilize
)(implicit
  inFunctor: ZeroFunctor[In],
  outFunctor: ZeroFunctor[Out]
) extends Optimizer[In, Out] {

  def apply[A](
    subspace: model.space.AllowedSubspace,
    data: RDD[(In[A], Out[A])],
    costFun: CostFun[In, Out],
    instUntyped: Any // subspace.Type[A]
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): model.Type[A] = {
    val sc = data.sparkContext
    val runner = SparkGD[In, Out, A](
      modelBC = sc.broadcast(model),
      costFunBC = sc.broadcast(costFun),
      diffEngineBC = sc.broadcast(diffEngine),
      flBC = sc.broadcast(fl)
    )
    val subspaceBC = sc.broadcast(
      subspace.asInstanceOf[runner.model.space.AllowedSubspace])(runner.model.space.subspaceClassTag)

    // Prepare the data for differentiation.
    implicit val z = runner.diffEngine.zero[A]
    val augData = data.map(sample => (
      inFunctor.map(sample._1)(runner.diffEngine.constant(_)),
      outFunctor.map(sample._2)(runner.diffEngine.constant(_))))
    data.cache()
    augData.cache()
    val count = data.count()

    val tr = gradTrans.create[subspace.Type, A]()(fl, subspace.space)
    var inst = instUntyped.asInstanceOf[subspace.Type[A]]
    var best = inst
    var bestCost = runner.totalCost(subspaceBC, data, inst)

    for (i <- 1 to iterations) {
      val gradSamples = runner.gradSamples(subspaceBC, augData, inst, count).asInstanceOf[subspace.Type[A]]
      val gradReg = runner.gradReg(subspace.asInstanceOf[runner.model.space.AllowedSubspace], inst).asInstanceOf[subspace.Type[A]]
      val grad = subspace.space.add[A](gradSamples, gradReg)
      inst = subspace.space.sub[A](inst, tr(grad))
      val cost = runner.totalCost(subspaceBC, data, inst)
      println(s"Iteration $i: ${cost}")

      if (cmp.lt(cost, bestCost)) {
        best = inst
        bestCost = cost
      }
    }

    subspace.inject(best)
  }
}
