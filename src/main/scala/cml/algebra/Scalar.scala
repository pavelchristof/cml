package cml.algebra

import cml.Enumerate
import cml.algebra.traits._

object Scalar extends Concrete[({type T[A] = A})#T] {
  type Type[A] = A

  override type Index = Unit

  override def enumerateIndex: Enumerate[Index] = Enumerate.unit

  override val dimFin: BigInt = 1

  override def tabulate[A](h: (Index) => A)(implicit additive: Additive[A]): A = h()

  override def index[A](v: A)(i: Index): A = v

  override def map[A, B](x: A)(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): B = f(x)

  override def apply2[A, B, C](x: A, y: B)(f: (A, B) => C)
      (implicit a: Additive[A], b: Additive[B], c: Additive[C]): C = f(x, y)

  override def ap[A, B](x: A)(f: (A) => B)(implicit a: Additive[A], b: Additive[B]): B = f(x)

  override def sum[F](v: F)(implicit f: Additive[F]): F = v
}
