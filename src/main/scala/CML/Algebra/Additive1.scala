package CML.Algebra

trait Additive1[V[_]] {
  def zero[F](implicit f: Field[F]): V[F]
  def add[F](x: V[F], y: V[F])(implicit f: Field[F]): V[F] = sub(x, neg(y))
  def sub[F](x: V[F], y: V[F])(implicit f: Field[F]): V[F] = add(x, neg(y))
  def neg[F](x: V[F])(implicit f: Field[F]): V[F] = sub(zero, x)
}
