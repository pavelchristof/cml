package cml.optimization

import cml._
import cml.algebra._
import org.apache.spark.rdd.RDD

import scala.util.Random

case class StochasticGradientDescent[In[_], Out[_]] (
  model: Model[In, Out],
  iterations: Int,
  gradTrans: GradTrans = Stabilize
)(implicit
  in: ZeroFunctor[In],
  out: ZeroFunctor[Out]
) extends Optimizer[In, Out] {

  def apply[A](
    batchesRDD: RDD[Seq[(In[A], Out[A])]],
    costFun: CostFun[In, Out],
    initialInst: model.Type[A]
  )(implicit
    fl: Floating[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): model.Type[A] = {
    import ZeroEndofunctor.asZero
    import diffEngine.zero

    val batches = batchesRDD.collect().toVector

    // This is the largest subspace that we'll work with.
    val subspace = model.restrictRDD(batchesRDD.flatMap(identity), costFun)
    implicit val space = subspace.space

    // Prepare the cost function.
    val costs = batches.map(data => model.cost(data, costFun))

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
        implicit val augAn = diffEngine.analytic(fl, ctx)
        val injectedInst = subspace.inject(batchSubspace.inject(inst))

        prepData
          .map(model.applySample(injectedInst)(_))
          .map(costFun.scoreSample[diffEngine.Aug[A]](_))
          .reduce(augAn.add(_, _))
      })

      val regGrad = diffEngine.grad[A, batchSubspace.Type](inst => implicit ctx => {
        import diffEngine.analytic
        costFun.regularization[batchSubspace.Type, diffEngine.Aug[A]](inst)
      })

      val count = data.size

      (inst: subspace.Type[A]) => {
        val projInst = batchSubspace.project(inst)
        val g = batchSpace.add(
          batchSpace.div(grad(projInst), fl.fromLong(count)),
          regGrad(projInst))
        batchSubspace.inject(g)
      }
    })

    // Get the sample count and create a new gradient transformer.
    val count = fl.fromLong(batches.flatMap(identity).size)
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
    val rng = new Random()

    for (i <- 1 to iterations) {
      val shuffled = rng.shuffle(batchGrad)

      for (g <- shuffled) {
        inst = space.sub(inst, tr(g(inst)))
      }

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
