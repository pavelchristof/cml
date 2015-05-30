package cml.algebra

import cml.algebra.traits._
import shapeless.Nat
import shapeless.ops.nat.ToInt
import scala.reflect.ClassTag

case class Vector[+S <: Nat, A] (
  get: Array[A]
) extends Serializable {
  override def toString: String = get.toVector.toString()
}

class VectorImpl[S <: Nat](implicit size: ToInt[S])
  extends Cartesian[({type T[a] = Vector[S, a]})#T]
  with Serializable {

  type Type[a] = Vector[S, a]

  /**
   * Constructs a vector from a sequence.
   */
  def from[A : ClassTag](vec: Seq[A]): Option[Vector[S, A]] =
    if (vec.size == size()) {
      Some(Vector(vec.toArray))
    } else {
      None
    }

  type Key = Int

  override val dim: Int = size()

  override def zero[A](implicit a: Zero[A]): Vector[S, A] =
    Vector(Array.fill(dim)(a.zero))

  override def map[A, B](v: Vector[S, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): Vector[S, B] =
    Vector(Array.tabulate(dim)(i => h(v.get(i))))

  override def apply2[A, B, C](x: Vector[S, A], y: Vector[S, B])(h: (A, B) => C)
      (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Vector[S, C] =
    Vector(Array.tabulate(dim)(i => h(x.get(i), y.get(i))))

  override def zip[A, B](x: Vector[S, A], y: Vector[S, B])
      (implicit a: Zero[A], b: Zero[B]): Vector[S, (A, B)] =
    Vector(Array.tabulate(dim)(i => (x.get(i), y.get(i))))

  override def ap[A, B](x: Vector[S, A])(h: Vector[S, (A) => B])
      (implicit a: Zero[A], b: Zero[B]): Vector[S, B] =
    Vector(Array.tabulate(dim)(i => h.get(i)(x.get(i))))

  override def point[A](x: A)(implicit a: Zero[A]): Vector[S, A] =
    Vector(Array.fill(dim)(x))

  override def tabulate[A](v: (Int) => A)(implicit a: Zero[A]): Vector[S, A] =
    Vector(Array.tabulate(dim)(v))

  override def index[A](v: Vector[S, A])(k: Int)(implicit a: Zero[A]): A =
    v.get(k)

  override def sum[A](v: Vector[S, A])(implicit a: Additive[A]): A =
    v.get.fold(a.zero)(a.add)
}

trait RuntimeNat {
  type Type <: Nat
  def apply(): ToInt[Type]
}

object RuntimeNat {
  def apply(n: Int): RuntimeNat = new RuntimeNat {
    assert(n >= 0)
    override type Type = Nat
    override def apply(): ToInt[Type] = new ToInt[Type] {
      override def apply(): Int = n
    }
  }
}

object Vector {
  def apply[S <: Nat](size: ToInt[S]) =
    new VectorImpl[S]()(size)

  def apply[S <: Nat](n: S)(implicit size: ToInt[S]) =
    new VectorImpl[S]
}
