import cml.algebra.traits.AnalyticMap
import cml.models._
import cml.algebra
import cml.algebra.Real._
import shapeless.Nat

import scala.util.Random

object ModelTest extends App {
  implicit val vecIn = algebra.Vector(Nat(5))
  implicit val vecHidden = algebra.Vector(Nat(20))
  implicit val vecOut = algebra.Vector(Nat(2))

  val model = Chain4(
    LinearMap[vecIn.Type, vecHidden.Type],
    Pointwise[vecHidden.Type](AnalyticMap.sigmoid),
    LinearMap[vecHidden.Type, vecOut.Type],
    Pointwise[vecOut.Type](AnalyticMap.sigmoid)
  )

  val rng = new Random()
  val instance = model.fill(rng.nextDouble)
  val input = vecIn.from(Seq(1.0, 2.0, 3.0, 4.0, 5.0)).get
  println(model(input)(instance))
}
