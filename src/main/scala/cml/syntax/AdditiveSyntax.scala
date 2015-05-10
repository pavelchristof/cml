package cml.syntax

import cml.algebra.traits._
import scalaz.syntax.Ops

final class AdditiveOps[F] private[syntax](val self: F)(implicit f: Additive[F]) extends Ops[F] {
  final def +(x: F): F = f.add(self, x)
  final def unary_+(): F = self
  final def -(x: F): F = f.sub(self, x)
  final def unary_-(): F = f.neg(self)
}

trait ToAdditiveOps {
  implicit def ToAdditiveOps[F](x: F)(implicit f: Additive[F]): AdditiveOps[F] = new AdditiveOps[F](x)
}

trait AdditiveSyntax[F] {
  implicit def ToAdditiveOps(v: F): AdditiveOps[F] = new AdditiveOps[F](v)(AdditiveSyntax.this.F)
  def F: Additive[F]
}
