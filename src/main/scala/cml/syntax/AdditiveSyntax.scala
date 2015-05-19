package cml.syntax

import cml.algebra.traits._
import scalaz.syntax.Ops

final class AdditiveOps[F] private[syntax](val self: F)(implicit f: Additive[F]) extends Ops[F] {
  def +(x: F): F = f.add(self, x)
  def unary_+(): F = self
  def -(x: F): F = f.sub(self, x)
  def unary_-(): F = f.neg(self)
}

trait ToAdditiveOps {
  implicit def ToAdditiveOps[F](x: F)(implicit f: Additive[F]): AdditiveOps[F] = new AdditiveOps[F](x)
}

trait AdditiveSyntax[F] {
  implicit def ToAdditiveOps(v: F): AdditiveOps[F] = new AdditiveOps[F](v)(AdditiveSyntax.this.F)

  def _0: F = F.zero

  def F: Additive[F]
}
