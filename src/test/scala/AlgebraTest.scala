import CML.Algebra.Real._
import CML.Algebra.{Additive, Additive1, Field, Sized}
import shapeless.Nat

object AlgebraTest extends App {
  def fun[F](x: F)(implicit f: Additive[F]): F = f.add(x, x)
  def fun2[V[_], F](x: V[F])(implicit s: Additive1[V], f: Field[F]): V[F] = s.add(x, x)

  println(fun[Double](2))

  val s = Sized(Nat(3))
  implicit val v = s.Vector
  val v1 = s.Vector.point(2.0)
  val v2 = s.Vector.point(3.0)
  println(s.Vector.add(v1, v2))
  println(fun2(v1))
}
