package cml.optimization

import cml._
import cml.algebra._
import org.apache.spark.rdd.RDD

/**
 * Distributed gradient descent.
 */
case class GradientDescent[In[_], Out[_]] (
  model: Model[In, Out],
  iterations: Int,
  gradTrans: GradTrans = Stabilize
)(implicit
  in: ZeroFunctor[In],
  out: ZeroFunctor[Out]
) extends Optimizer[In, Out] {

  def apply[A](
    batches: RDD[Seq[(In[A], Out[A])]],
    costFun: CostFun[In, Out],
    initialInst: model.Type[A]
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): model.Type[A] = {
    import ZeroEndofunctor.asZero
    import diffEngine.{zero, analytic}

    // This is the largest subspace that we'll work with.
    val subspace = model.restrictRDD(batches.flatMap(identity), costFun)
    implicit val space = subspace.space

    val sc = batches.sparkContext
    val subspaceBC = sc.broadcast(subspace)
    
    // Prepare the cost function.
    val costs = batches.map(data => model.cost(data, costFun)).cache()

    // Prepare the regularization function.
    def reg(inst: subspace.Type[A]) =
      costFun.regularization[subspace.Type, A](inst)

    // Prepare the batch gradients.
    val batchGrad = batches.map(data => {
      // Restrict the subspace further.
      val keys = data
        .map(sample => {
          type R = Reflector[space.Key]
          val converted = model.convertSample[A, R](sample)
          space.reflect(inst => {
            val sample = Sample(
              input = converted._1,
              expected = converted._2,
              actual = model(subspace.inject(inst))(converted._1)
            )
            costFun.scoreSample(sample)
          })
        })
        .reduce(_ ++ _)
      val batchSubspace = space.restrict(keys)
      implicit val batchSpace = batchSubspace.space

      // Wrap numbers in data with Aug[_].
      val prepData = data.map(s => (
        in.map(s._1)(diffEngine.constant(_)),
        out.map(s._2)(diffEngine.constant(_))))

      val grad = diffEngine.grad[A, batchSubspace.Type](inst => implicit ctx => {
        val injectedInst = subspace.inject(batchSubspace.inject(inst))

        prepData
          .map(model.applySample(injectedInst)(_))
          .map(costFun.scoreSample[diffEngine.Aug[A]](_))
          .reduce(analytic(fl, ctx).add(_, _))
      })

      (inst: subspace.Type[A]) => batchSubspace.inject(grad(batchSubspace.project(inst)))
    }).cache()

    // Prepare the regularization gradient.
    val regGrad = diffEngine.grad[A, subspace.Type](inst => implicit ctx => {
      costFun.regularization[subspace.Type, diffEngine.Aug[A]](inst)
    })

    // Get the sample count and create a new gradient transformer.
    val count = fl.fromLong(batches.flatMap(identity).count())
    val tr = gradTrans.create[subspace.Type, A]()

    def totalCost(inst: subspace.Type[A]): A =
      fl.add(
        fl.div(costs.map(f => f(subspace.inject(inst))).reduce(fl.add(_, _)), count),
        reg(inst))

    println(s"Model dimension: ${space.dim}")

    var inst = subspace.project(initialInst)
    var cost = totalCost(inst)
    var bestInst = inst
    var bestCost = cost

    for (i <- 1 to iterations) {
      val gradBatches = batchGrad
        .map(grad => grad(inst): Any)
        .reduce((x: Any, y: Any) => {
          val ss = subspaceBC.value
          ss.space.add(x.asInstanceOf[ss.Type[A]], y.asInstanceOf[ss.Type[A]]): Any
        })
        .asInstanceOf[subspace.Type[A]]
      val gradTotal = space.add(
        space.div(gradBatches, count),
        regGrad(inst))

      inst = space.sub(inst, tr(gradTotal))
      cost = totalCost(inst)

      println(s"Iteration $i: $cost")

      if (!fl.isNaN(cost) && cmp.lt(cost, bestCost)) {
        bestInst = inst
        bestCost = cost
      }
    }

    subspace.inject(bestInst)
  }
}
