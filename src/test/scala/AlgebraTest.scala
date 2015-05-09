import CML.Algebra.Real._
import CML.Algebra.{Additive, Additive1, Field, Vector}
import shapeless.{Succ, _0, Nat}

object AlgebraTest extends App {
  def fun[F](x: F)(implicit f: Additive[F]): F = f.add(x, x)
  def fun2[V[_], F](x: V[F])(implicit s: Additive1[V], f: Field[F]): V[F] = s.add(x, x)

  println(fun[Double](2))

  implicit val i = Vector(Nat(3))
  val i2 = Vector(Nat(3))
  val v = i2.point(2.0)
  val v2 = i.add(v, v)
  print(fun2[({type λ[α] = Vector[Succ[Succ[Succ[_0]]], α]})#λ, Double](v2))
}
