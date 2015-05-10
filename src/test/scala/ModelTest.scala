import cml.models._
import cml.algebra
import cml.algebra.Real._
import shapeless.Nat

import scala.util.Random

object ModelTest extends App {
  implicit val vec5 = algebra.Vector(Nat(5))
  implicit val vec10 = algebra.Vector(Nat(10))
  implicit val vec1 = algebra.Vector(Nat(1))

  implicit val model = Chain(
    LinearMap[vec5.Type, vec10.Type],
    LinearMap[vec10.Type, vec1.Type]
  )

  val rng = new Random()
  val instance = model.fill(rng.nextDouble)
  val input = vec5.from(Seq(1.0, 2.0, 3.0, 4.0, 5.0)).get
  println(model(input)(instance))
}
