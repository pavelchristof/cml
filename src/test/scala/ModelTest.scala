import cml._
import cml.algebra._
import cml.models._
import cml.optimization._
import Cartesian._
import Floating._

import scala.util.Random
import scalaz._

object ModelTest extends App {
  implicit val diffEngine = ad.Backward

  val vecSize = RuntimeNat(1)

  type VecIn[A] = A
  type VecHidden[A] = Vec[vecSize.Type, A]
  type VecOut[A] = A
  type VecTree[A] = Tree[VecIn[A]]

  implicit val vecHiddenSpace = Cartesian.vec(vecSize())

  val model = Chain4(
    AffineMap[VecIn, VecHidden],
    Pointwise[VecHidden](AnalyticMap.sin),
    AffineMap[VecHidden, VecOut],
    Pointwise[VecOut](AnalyticMap.sigmoid)
  )

  val costFun = new CostFun[VecIn, VecOut] {
    override def scoreSample[A](sample: Sample[VecIn[A], VecOut[A]])(implicit a: Analytic[A]): A = {
      import a.analyticSyntax._
      val eps = fromDouble(0.0001)
      val ex = sample.expected
      val ac = sample.actual
      - (ex * (ac + eps).log + (_1 - ex) * (_1 - ac + eps).log)
    }

    override def regularization[V[_], A](inst: V[A])(implicit a: Analytic[A], space: Normed[V]): A = {
      import a.analyticSyntax._
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
      subspace = optimizer.model.space.restrict(Set.empty[optimizer.model.space.Key]),
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
