package cml.algebra

abstract class ZeroNatIso[F[_], G[_]] {
  def apply[A](v: F[A])(implicit a: Zero[A]): G[A]
  def unapply[A](u: G[A])(implicit a: Zero[A]): F[A]
}

object CartesianNatIso {
  def apply[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new ZeroNatIso[F, G] {
    assert(f.dim == g.dim)

    override def apply[A](v: F[A])(implicit a: Zero[A]): G[A] =
      g.tabulate(i => f.index(v)(f.intToKey(g.keyToInt(i))))
    override def unapply[A](u: G[A])(implicit a: Zero[A]): F[A] =
      f.tabulate(i => g.index(u)(g.intToKey(f.keyToInt(i))))
  }
}
