package cml.syntax

import cml.algebra.traits._
import scalaz.syntax.Ops

final class FieldOps[F] private[syntax](val self: F)(implicit f: Field[F]) extends Ops[F] {
  final def /(x: F): F = f.div(self, x)
}

trait ToFieldOps extends ToRingOps {
  implicit def ToFieldOps[F](x: F)(implicit f: Field[F]): FieldOps[F] = new FieldOps[F](x)
}

trait FieldSyntax[F] extends RingSyntax[F] {
  implicit def ToFieldOps(v: F): FieldOps[F] = new FieldOps[F](v)(FieldSyntax.this.F)
  def F: Field[F]
}
