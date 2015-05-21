import cml._
import cml.algebra.traits._
import cml.algebra.Instances._

object LCADTest extends App {
  implicit val space = algebra.Map.locallyConcrete[BigInt](Enumerate.natural)

  def fun[A](v: Map[BigInt, A])(implicit an: Analytic[A]): A = {
    import an.analyticSyntax._
    space.indexLC(v)(5) + space.indexLC(v)(3) + space.length(v)
  }

  val vec = Map(BigInt(0) -> 1., BigInt(1) -> 2., BigInt(3) -> 4.)
  val subspace = space.restrict[Double](fun[Double](_))(vec)

  println(subspace.concrete)

  import ad.Backward._

  def err(v: Map[BigInt, Aug[Double]], ctx: Context[Double]): Aug[Double] = {
    implicit val an: Analytic[Aug[Double]] = analytic(implicitly, ctx)
    import an.analyticSyntax._
    space.indexLC(v)(42).square - fromInt(100) + space.indexLC(v)(13) * fromInt(12)
  }

  val g = gradLC[Double, ({type T[A] = Map[BigInt, A]})#T](err)
  println(g(Map()))
  println(g(Map(BigInt(42) -> 100.0)))
  println(g(Map(BigInt(42) -> 50.0)))
}
