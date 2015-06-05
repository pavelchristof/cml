package cml.algebra

import shapeless.Nat
import shapeless.ops.nat.ToInt

case class Vec[S <: Nat, A] (
  get: Array[A]
) extends Serializable {
  override def toString: String = get.toVector.toString()
}

object Vec {
  implicit class CartesianImpl[S <: Nat](size: ToInt[S])
    extends Cartesian[({type T[a] = Vec[S, a]})#T] {
    type Key = Int

    override val dim: Int = size()

    override def keyToInt(k: Int): Int = k

    override def intToKey(i: Int): Int = i

    override def zero[A](implicit a: Zero[A]): Vec[S, A] =
      Vec(Array.fill(dim)(a.zero))

    override def map[A, B](v: Vec[S, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): Vec[S, B] =
      Vec(Array.tabulate(dim)(i => h(v.get(i))))

    override def apply2[A, B, C](x: Vec[S, A], y: Vec[S, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Vec[S, C] =
      Vec(Array.tabulate(dim)(i => h(x.get(i), y.get(i))))

    override def zip[A, B](x: Vec[S, A], y: Vec[S, B])
        (implicit a: Zero[A], b: Zero[B]): Vec[S, (A, B)] =
      Vec(Array.tabulate(dim)(i => (x.get(i), y.get(i))))

    override def ap[A, B](x: Vec[S, A])(h: Vec[S, (A) => B])
        (implicit a: Zero[A], b: Zero[B]): Vec[S, B] =
      Vec(Array.tabulate(dim)(i => h.get(i)(x.get(i))))

    override def point[A](x: A)(implicit a: Zero[A]): Vec[S, A] =
      Vec(Array.fill(dim)(x))

    override def tabulate[A](v: (Int) => A)(implicit a: Zero[A]): Vec[S, A] =
      Vec(Array.tabulate(dim)(v))

    override def index[A](v: Vec[S, A])(k: Int)(implicit a: Zero[A]): A =
      v.get(k)

    override def sum[A](v: Vec[S, A])(implicit a: Additive[A]): A =
      v.get.fold(a.zero)(a.add)

    override def restrict(keys: => Set[Int]): Cartesian[({type T[a] = Vec[S, a]})#T] = this
  }

  implicit def cartesian[S <: Nat](implicit toInt: ToInt[S]) = new CartesianImpl[S](toInt)
}
