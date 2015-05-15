import cml.algebra
import cml.algebra.traits._
import cml.algebra.Real._
import cml.models._
import shapeless.Nat

import scala.util.Random
import scalaz._

object ModelTest extends App {
  implicit val vecWord = algebra.Vector(Nat(40))
  implicit val vecPair = algebra.Product[vecWord.Type, vecWord.Type]
  implicit val vecHidden = algebra.Vector(Nat(20))
  implicit val vecOut = algebra.Vector(Nat(3))
  type VecTree[A] = algebra.Compose[Tree, vecWord.Type]#Type[A]

  val model = Chain5(
    Reduce[Tree, vecWord.Type](
      LinearMap[vecPair.Type, vecWord.Type]
    ) : Model[VecTree, vecWord.Type],
    LinearMap[vecWord.Type, vecHidden.Type],
    Pointwise[vecHidden.Type](AnalyticMap.sigmoid),
    LinearMap[vecHidden.Type, vecOut.Type],
    Softmax[vecOut.Type]
  )

  val rng = new Random()
  def sign() = if (rng.nextBoolean()) 1 else -1

  val inst = model.fill(sign * rng.nextDouble)
  val tree = Tree.node(vecWord.point(4.0), Stream(
    Tree.leaf(vecWord.point(2.0)),
    Tree.leaf(vecWord.point(-1.0))
  ))

  println(inst)
  println(model(tree)(inst))
}
