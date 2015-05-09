package CML.Algebra

trait Normed[V[_]] extends Linear[V] {
  def norm[F](v: V[F])(implicit f: Field[F]): F = dist(v, v)
  def dist[F](u: V[F], v: V[F])(implicit f: Field[F]): F = norm(sub(u, v))
  def dot[F](u: V[F], v: V[F])(implicit f: Field[F]): F
  def quadrance[F](v: V[F])(implicit f: Field[F]): F = dot(v, v)
}
