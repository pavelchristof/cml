package cml.algebra

import scala.reflect.ClassTag

abstract class NatIso[F[_], G[_]] {
  def apply[A](v: F[A])(implicit a: ClassTag[A]): G[A]
  def unapply[A](u: G[A])(implicit a: ClassTag[A]): F[A]
}

object NatIso {
  def apply[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new NatIso[F, G] {
    assert(f.dim == g.dim)

    override def apply[A](v: F[A])(implicit a: ClassTag[A]): G[A] =
      g.tabulate(i => f.index(v)(f.intToKey(g.keyToInt(i))))
    override def unapply[A](u: G[A])(implicit a: ClassTag[A]): F[A] =
      f.tabulate(i => g.index(u)(g.intToKey(f.keyToInt(i))))
  }
}
