package cml.algebra.traits

trait Linear[V[_]] extends Additive1[V] {
  def mull[F](a: F, v: V[F])(implicit f: Field[F]): V[F]
  def mulr[F](v: V[F], a: F)(implicit f: Field[F]): V[F]
  def div[F](v: V[F], a: F)(implicit f: Field[F]): V[F]
}
