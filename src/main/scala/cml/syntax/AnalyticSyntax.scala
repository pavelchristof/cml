package cml.syntax

import cml.algebra.traits._
import scalaz.syntax.Ops

final class AnalyticOps[F] private[syntax](val self: F)(implicit f: Analytic[F]) extends Ops[F] {
  final def abs: F = f.abs(self)
  final def signum: F = f.signum(self)

  final def exp: F = f.exp(self)
  final def log: F = f.log(self)
  final def sqrt: F = f.sqrt(self)

  final def sin: F = f.sin(self)
  final def cos: F = f.cos(self)
  final def tan: F = f.tan(self)

  final def asin: F = f.asin(self)
  final def acos: F = f.acos(self)
  final def atan: F = f.atan(self)

  final def sinh: F = f.sinh(self)
  final def cosh: F = f.cosh(self)
  final def tanh: F = f.tanh(self)
}

trait ToAnalyticOps extends ToFieldOps {
  implicit def ToAnalyticOps[F](x: F)(implicit f: Analytic[F]): AnalyticOps[F] = new AnalyticOps[F](x)
}

trait AnalyticSyntax[F] extends FieldSyntax[F] {
  implicit def ToAnalyticOps(v: F): AnalyticOps[F] = new AnalyticOps[F](v)(AnalyticSyntax.this.F)
  def F: Analytic[F]
}
