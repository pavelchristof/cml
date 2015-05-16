package cml.syntax

import cml.algebra.traits._
import scalaz.syntax.Ops

final class RingOps[F] private[syntax](val self: F)(implicit f: Ring[F]) extends Ops[F] {
  def *(x: F): F = f.mul(self, x)
  def square: F = f.mul(self, self)
}

trait ToRingOps extends ToAdditiveOps {
  implicit def ToRingOps[F](x: F)(implicit f: Ring[F]): RingOps[F] = new RingOps[F](x)
}

trait RingSyntax[F] extends AdditiveSyntax[F] {
  implicit def ToRingOps(v: F): RingOps[F] = new RingOps[F](v)(RingSyntax.this.F)
  def F: Ring[F]
}
