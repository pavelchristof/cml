import cml._
import cml.algebra.traits._
import cml.algebra.Real._
import shapeless.Nat

object LCADTest extends App {
  implicit val space = algebra.Map.locallyConcrete[BigInt](Enumerate.natural)

  def fun[A](v: Map[BigInt, A])(implicit an: Analytic[A]): A = {
    import an.analyticSyntax._
    space.indexLC(v)(5) + space.indexLC(v)(3) + space.length(v)
  }

  val vec = Map(BigInt(0) -> 1., BigInt(1) -> 2., BigInt(3) -> 4.)
  val concrete = space.restrict[Double](fun[Double](_))(vec)

  println(concrete.point(()).keySet)

  def err[A](v: Map[BigInt, A])(implicit an: Analytic[A]): A = {
    import an.analyticSyntax._
    space.indexLC(v)(42).square - fromInt(100) + space.indexLC(v)(13) * fromInt(12)
  }

  import ad.Forward._
  val g = gradLC[Double, ({type T[A] = Map[BigInt, A]})#T](err[Aug[Double]])
  println(g(Map().withDefault(_ => 0.0)))
  println(g(Map(BigInt(42) -> 100.0).withDefault(_ => 0.0)))
  println(g(Map(BigInt(42) -> 50.0).withDefault(_ => 0.0)))
}
