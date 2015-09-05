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

  val vecSize = RuntimeNat(30)

  type VecIn[A] = A
  type VecHidden[A] = Vec[vecSize.Type, A]
  type VecOut[A] = A

  implicit val vecHiddenSpace = Vec.cartesian(vecSize())

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
      fromDouble(0) * space.quadrance(inst)
    }
  }

  val data = Vector(
    Seq((1d, 1d)),
    Seq((2d, 0d)),
    Seq((3d, 0.5d)),
    Seq((4d, 0d)),
    Seq((5d, 1d)),
    Seq((6d, 0d)),
    Seq((7d, 0.5d)),
    Seq((8d, 0d))
  )

  val optimizer = StochasticGradientDescent(
    iterations = 1000,
    gradTrans = Stabilize.andThen(AdaGrad)
  )

  var rng = new Random()
  val initialInst = model.params.tabulate(_ => rng.nextDouble() * 2 - 1)

  val learned = optimizer(model)(data, costFun, initialInst)

  for (i <- 1 to 16) {
    println(model(learned)(i.toDouble))
  }
}
