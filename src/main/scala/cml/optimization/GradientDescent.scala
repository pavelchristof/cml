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
  inFunctor: ZeroFunctor[In],
  outFunctor: ZeroFunctor[Out]
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

    implicit val space = model.restrictRDD(batches.flatMap(identity), costFun)
    val gradients = batches.map(data => {
      val space = model.restrict(data, costFun)
      println(s"Dim: ${space.dim}")
      model.gradient(data, costFun)(space)
    }).cache()
    val costs = batches.map(data => model.cost(data, costFun)).cache()
    val reg = model.reg(costFun)(space)
    val regGrad = model.regGradient(costFun)(space)
    val count = fl.fromLong(batches.flatMap(identity).count())
    val tr = gradTrans.create[model.Type, A]()(fl, space)

    def totalCost(inst: model.Type[A]): A =
      fl.add(
        fl.div(costs.map(f => f(inst)).reduce(fl.add(_, _)), count),
        reg(inst))

    println(s"Model dimension: ${space.dim}")

    var inst = initialInst
    var cost = totalCost(inst)
    var bestInst = inst
    var bestCost = cost

    for (i <- 1 to iterations) {
      val gradBatches = gradients
        .map(g => g(inst))
        .reduce(model.space.add(_, _))
      val gradReg = regGrad(inst)
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
