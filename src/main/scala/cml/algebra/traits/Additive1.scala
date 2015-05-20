package cml.algebra.traits

trait Additive1[V[_]] {
  def zero[F](implicit f: Additive[F]): V[F]
  def add[F](x: V[F], y: V[F])(implicit f: Additive[F]): V[F]
  def sub[F](x: V[F], y: V[F])(implicit f: Additive[F]): V[F] = add(x, neg(y))
  def neg[F](x: V[F])(implicit f: Additive[F]): V[F]

  implicit def additive[F](implicit f: Additive[F]): Additive[V[F]] = new Additive[V[F]] {
    override val zero: V[F] = Additive1.this.zero
    override def neg(x: V[F]): V[F] = Additive1.this.neg(x)
    override def add(x: V[F], y: V[F]): V[F] = Additive1.this.add(x, y)

    override def runtimeClass: Class[_] = zero.getClass
  }
}
