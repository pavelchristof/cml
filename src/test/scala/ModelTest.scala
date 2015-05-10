import CLM.Models._
import CML.Algebra._
import CML.Algebra.Real._
import shapeless.Nat

import scala.util.Random

object ModelTest extends App {
  implicit val vec5 = Vector(Nat(5))
  implicit val vec10 = Vector(Nat(10))
  implicit val vec1 = Vector(Nat(1))

  implicit val model = Compose[
    ({type T[F] = LinearMapModel[vec5.Type, vec10.Type, F]})#T,
    ({type T[F] = LinearMapModel[vec10.Type, vec1.Type, F]})#T
    ]()(
    LinearMap[vec5.Type, vec10.Type](),
    LinearMap[vec10.Type, vec1.Type]()
  )

  import model.functorSyntax._

  val rng = new Random()
  val inst: model.Type[Double] = model.fill(rng.nextDouble)
  println(model.predict(vec5.point(3.0), inst))
  println(inst)
}
