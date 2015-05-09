import CML.Algebra.Real._
import CML.Algebra._
import shapeless.ops.nat.ToInt
import shapeless.{Succ, _0, Nat}

object AlgebraTest extends App {
  def fun[F](x: F)(implicit f: Additive[F]): F = f.add(x, x)
  def fun2[V[_], F](x: V[F])(implicit s: Additive1[V], f: Field[F]): V[F] = s.add(x, x)
  println(fun[Double](2))

  implicit val vec2 = Vector(Nat(2))
  implicit val vec3 = Vector(Nat(3))
  val vec2b = Vector(Nat(2))

  println(vec2.dim)
  println(vec3.dim)

  val u: vec3.Type[Double] = vec3.point(2.0)
  val v = vec3.mull(3.0, u)
  println(u)
  println(v)

  val w = vec2.point(1.0)
  val r = vec2.add(w, vec2b.point(3.0))
  println(w)

  println(fun2[vec2.Type, Double](w))
}
