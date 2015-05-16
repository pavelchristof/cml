import cml._
import cml.algebra.ad
import cml.algebra.traits._
import cml.algebra.Real._
import cml.models._
import cml.optimization.GradientDescent
import shapeless.Nat

import scalaz._

object ModelTest extends App {
  implicit val diffEngine = ad.Forward
  implicit val vecWord = algebra.Vector(Nat(10))
  implicit val vecPair = algebra.Product[vecWord.Type, vecWord.Type]
  implicit val vecHidden = algebra.Vector(Nat(5))
  implicit val vecOut = algebra.Scalar
  implicit val vecTree = algebra.Compose[Tree, vecWord.Type]

  val model = Chain5(
    Reduce[Tree, vecWord.Type](
      LinearMap[vecPair.Type, vecWord.Type]
    ) : Model[vecTree.Type, vecWord.Type],
    LinearMap[vecWord.Type, vecHidden.Type],
    Pointwise[vecHidden.Type](AnalyticMap.sigmoid),
    LinearMap[vecHidden.Type, vecOut.Type],
    Pointwise[vecOut.Type](AnalyticMap.sigmoid)
  )

  val costFun = new CostFun[vecTree.Type, vecOut.Type] {
    override def scoreSample[A](sample: ScoredSample[vecTree.Type[A], vecOut.Type[A]])(implicit an: Analytic[A]): A = {
      import an.analyticSyntax._
      (sample.actual - sample.expected).square
    }

    override def regularization[V[_], A](inst: V[A])(implicit an: Analytic[A], space: LocallyConcrete[V]): A = {
      import an.analyticSyntax._
      0
    }
  }

  val tree = Tree.node(vecWord.point(4.0), Stream(
    Tree.leaf(vecWord.point(2.0)),
    Tree.leaf(vecWord.point(-1.0))
  ))
  val data = Vector(
    (tree, 0.2)
  )

  val optimizer = GradientDescent(model,
    iterations = 500,
    step = 0.001
  )(vecTree.functor, vecOut)

  optimizer[Double](Vector.empty, data, costFun) match {
    case Vector(learned) => println(model(learned.asInstanceOf[model.Type[Double]])(tree))
  }
}
