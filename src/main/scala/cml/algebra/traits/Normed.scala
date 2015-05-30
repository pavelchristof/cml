package cml.algebra.traits

trait Normed[V[_]] extends LocallyConcrete[V] {
  def sum[F](v: V[F])(implicit f: Additive[F]): F

  final def taxicab[F](v: V[F])(implicit f: Analytic[F]): F
    = sum(map(v)(f.abs(_)))
  final def length[F](v: V[F])(implicit f: Analytic[F]): F
    = f.sqrt(quadrance(v))
  final def dist[F](u: V[F], v: V[F])(implicit f: Analytic[F]): F
    = length(sub(u, v))
  final def dot[F](u: V[F], v: V[F])(implicit f: Field[F]): F
    = sum(apply2(u, v)(f.mul(_, _)))
  final def quadrance[F](v: V[F])(implicit f: Field[F]): F
    = dot(v, v)
}
