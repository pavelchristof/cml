import java.io.{FileInputStream, ObjectInputStream, FileOutputStream, ObjectOutputStream}

import cml.algebra.traits._
import cml.models._
import cml.algebra
import cml.algebra.Real._
import cml.algebra.ad.Forward._
import shapeless.Nat
import scala.util.Random

object ModelTest extends App {
  implicit val vecIn = algebra.Vector(Nat(5))
  implicit val vecHidden = algebra.Vector(Nat(20))
  implicit val vecOut = algebra.Vector(Nat(1))

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

  // Serialization
  val oos = new ObjectOutputStream(new FileOutputStream("obj.bin"))
  oos.writeObject(instance)
  oos.close

  val ois = new ObjectInputStream(new FileInputStream("obj.bin"))
  val loadedInstance = ois.readObject().asInstanceOf[model.Type[Double]]
  ois.close

  println(model(input)(loadedInstance))

  // Gradient
  import model._
  import vecIn.functorSyntax._

  val gradInput = input.map(inject(_))
  val gradient = grad[Double, model.Type](modelInst =>
    vecOut.index(model(gradInput)(modelInst))(0))_

  println(gradient(instance))
}
