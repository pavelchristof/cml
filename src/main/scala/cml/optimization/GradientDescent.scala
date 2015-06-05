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
    import diffEngine.zero

    // Prepare the cost function.
    val costs = batches.map(data => model.cost(data, costFun)).cache()

    // Prepare the regularization function.
    val regSubspace = model.restrictRDD(batches.flatMap(identity), costFun)
    implicit val regSpace = regSubspace.space
    def reg(inst: model.Type[A]) =
      costFun.regularization[regSubspace.Type, A](regSubspace.project(inst))

    // Prepare the batch gradients.
    val batchGrad = batches.map(data => {
      // Find the subspace just on this batch.
      val batchSubspace = model.restrict(data, costFun)
      implicit val batchSpace = batchSubspace.space

      // Wrap numbers in data with Aug[_].
      val prepData = data.map(s => (
        in.map(s._1)(diffEngine.constant(_)),
        out.map(s._2)(diffEngine.constant(_))))

      // Compute the gradient function.
      val grad = diffEngine.grad[A, batchSubspace.Type]((inst, ctx) => {
        implicit val augAn = diffEngine.analytic(fl, ctx)
        prepData
          .map(model.applySample(batchSubspace.inject(inst))(_))
          .map(costFun.scoreSample[diffEngine.Aug[A]](_))
          .reduce(augAn.add(_, _))
      })

      (inst: model.Type[A]) => batchSubspace.inject(grad(batchSubspace.project(inst)))
    }).cache()

    // Prepare the regularization gradient.
    val regGrad = diffEngine.grad[A, regSubspace.Type]((inst, ctx) => {
      implicit val augAn = diffEngine.analytic(fl, ctx)
      costFun.regularization[regSubspace.Type, diffEngine.Aug[A]](inst)
    })

    // Get the sample count and create a new gradient transformer.
    val count = fl.fromLong(batches.flatMap(identity).count())
    val tr = gradTrans.create[model.Type, A]()(fl, model.space)

    def totalCost(inst: model.Type[A]): A =
      fl.add(
        fl.div(costs.map(f => f(inst)).reduce(fl.add(_, _)), count),
        reg(inst))

    println(s"Model dimension: ${regSpace.dim}")

    var inst = initialInst
    var cost = totalCost(inst)
    var bestInst = inst
    var bestCost = cost

    for (i <- 1 to iterations) {
      val gradBatches = batchGrad
        .map(grad => grad(inst))(asZero(model.space, fl))
        .reduce(model.space.add(_, _))
      val gradReg = regSubspace.inject(regGrad(regSubspace.project(inst)))
      val gradTotal = model.space.add(
        model.space.div(gradBatches, count),
        gradReg)

      inst = model.space.sub(inst, tr(gradTotal))
      cost = totalCost(inst)

      println(s"Iteration $i: $cost")

      if (cmp.lt(cost, bestCost)) {
        bestInst = inst
        bestCost = cost
      }
    }

    bestInst
  }
}
