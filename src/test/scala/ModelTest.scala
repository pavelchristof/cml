import cml._
import cml.ad
import cml.algebra.traits._
import cml.algebra.Instances._
import cml.models._
import cml.optimization._
import shapeless.Nat

import scalaz.Scalaz._
import scalaz._

object ModelTest extends App {
  implicit val diffEngine = ad.Backward
  implicit val vecIn = algebra.Scalar
  implicit val vecHidden = algebra.Vector(Nat(1))
  val vecOut = algebra.Scalar
  implicit val vecTree = algebra.Compose[Tree, vecIn.Type]

  val model = Chain4(
    //Reduce[Tree, vecIn.Type](
    //  LinearMap[vecPair.Type, vecIn.Type]
    //) : Model[vecTree.Type, vecIn.Type],
    AffineMap[vecIn.Type, vecHidden.Type],
    Pointwise[vecHidden.Type](AnalyticMap.sin),
    AffineMap[vecHidden.Type, vecOut.Type],
    Pointwise[vecOut.Type](AnalyticMap.sigmoid)
  )

  val costFun = new CostFun[vecIn.Type, vecOut.Type] {
    override def scoreSample[A](sample: ScoredSample[vecIn.Type[A], vecOut.Type[A]])(implicit an: Analytic[A]): A = {
      import an.analyticSyntax._
      val eps = fromDouble(0.0001)
      val e = sample.expected
      val a = sample.actual
      - (e * (a + eps).log + (_1 - e) * (_1 - a + eps).log)
    }

    override def regularization[V[_], A](inst: V[A])(implicit an: Analytic[A], space: LocallyConcrete[V]): A = {
      import an.analyticSyntax._
      space.quadrance(inst) / fromInt(3000)
    }
  }

  val data = Vector(
    (1d, 1d),
    (2d, 0d),
    (3d, 1d),
    (4d, 0d)
  )

  val optimizer = new GradientDescent(
    model,
    iterations = 1000
  )(vecIn, vecOut) {
    override def newGradTrasnformer[A]()(implicit fl: Floating[A]): GradTransformer[model.Type, A] =
      Stabilize[model.Type, A]
        .andThen(AdaGrad[model.Type, A])
        .andThen(Scale[model.Type, A](fl.fromDouble(1)))
  }

  optimizer[Double](Vector.empty, data, costFun) match {
    case Vector(learned) => for (i <- Array(1d, 2d, 3d, 4d)) {
      println(model(learned.asInstanceOf[model.Type[Double]])(i))
    }
  }
}
