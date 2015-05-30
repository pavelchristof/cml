import cml._
import cml.algebra.{AnalyticMap, Analytic}
import cml.algebra.Instances._
import cml.algebra.traits._
import cml.models._
import cml.optimization._
import shapeless.Nat

import scala.util.Random
import scalaz._

object ModelTest extends App {
  implicit val diffEngine = ad.Backward
  implicit val vecIn = algebra.Scalar
  implicit val vecHidden = algebra.Vec(Nat(1))
  val vecOut = algebra.Scalar
  implicit val vecTree = algebra.Compose[Tree, vecIn.Type]

  val model = Chain4(
    AffineMap[vecIn.Type, vecHidden.Type],
    Pointwise[vecHidden.Type](AnalyticMap.sin),
    AffineMap[vecHidden.Type, vecOut.Type],
    Pointwise[vecOut.Type](AnalyticMap.sigmoid)
  )

  val costFun = new CostFun[vecIn.Type, vecOut.Type] {
    override def scoreSample[A](sample: Sample[vecIn.Type[A], vecOut.Type[A]])(implicit an: Analytic[A]): A = {
      import an.analyticSyntax._
      val eps = fromDouble(0.0001)
      val e = sample.expected
      val a = sample.actual
      - (e * (a + eps).log + (_1 - e) * (_1 - a + eps).log)
    }

    override def regularization[V[_], A](inst: V[A])(implicit an: Analytic[A], space: Concrete[V]): A = {
      import an.analyticSyntax._
      fromDouble(0.001) * space.quadrance(inst)
    }
  }

  val data = Seq(
    (1d, 1d),
    (2d, 0d),
    (3d, 1d),
    (4d, 0d)
  )

  var rng = new Random()

  val optimizer = MultiOpt(
    populationSize = 16,
    optimizer = GradientDescent(
      model,
      iterations = 1000,
      gradTrans = Stabilize.andThen(AdaGrad)
    )
  )

  val learned = optimizer[Double](
      population = Vector(),
      subspace = optimizer.model.space.restrict(Set.empty[optimizer.model.space.Index]),
      data = data,
      costFun = costFun,
      noise = rng.nextDouble() * 2 - 1)
    .minBy(_._1)
    ._2
    .asInstanceOf[model.Type[Double]]

  for (i <- Array(1d, 2d, 3d, 4d)) {
    println(model(learned)(i))
  }
}
