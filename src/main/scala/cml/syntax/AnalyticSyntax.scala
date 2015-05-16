package cml.syntax

import cml.algebra.traits._
import scalaz.syntax.Ops

final class AnalyticOps[F] private[syntax](val self: F)(implicit f: Analytic[F]) extends Ops[F] {
  def abs: F = f.abs(self)
  def signum: F = f.signum(self)

  def exp: F = f.exp(self)
  def log: F = f.log(self)
  def sqrt: F = f.sqrt(self)

  def sin: F = f.sin(self)
  def cos: F = f.cos(self)
  def tan: F = f.tan(self)

  def asin: F = f.asin(self)
  def acos: F = f.acos(self)
  def atan: F = f.atan(self)

  def sinh: F = f.sinh(self)
  def cosh: F = f.cosh(self)
  def tanh: F = f.tanh(self)
}

trait ToAnalyticOps extends ToFieldOps {
  implicit def ToAnalyticOps[F](x: F)(implicit f: Analytic[F]): AnalyticOps[F] = new AnalyticOps[F](x)
}

trait AnalyticSyntax[F] extends FieldSyntax[F] {
  implicit def ToAnalyticOps(v: F): AnalyticOps[F] = new AnalyticOps[F](v)(AnalyticSyntax.this.F)
  def FromDouble(x: Double): F = F.fromDouble(x)
  def F: Analytic[F]
}
