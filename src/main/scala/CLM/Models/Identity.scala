package CLM.Models

import CML.Algebra.{Analytic, Field}

case class Identity[F] () extends Serializable

class IdentityModel[V[_]] extends Model[Identity, V, V] {
  override def predict[F](input: V[F], model: Identity[F])(implicit f: Analytic[F]): V[F] = input

  override def div[F](v: Identity[F], a: F)(implicit f: Field[F]): Identity[F] = Identity()
  override def mull[F](a: F, v: Identity[F])(implicit f: Field[F]): Identity[F] = Identity()
  override def mulr[F](v: Identity[F], a: F)(implicit f: Field[F]): Identity[F] = Identity()
  override def zero[F](implicit f: Field[F]): Identity[F] = Identity()
  override def add[F](x: Identity[F], y: Identity[F])(implicit f: Field[F]): Identity[F] = Identity()
  override def neg[F](x: Identity[F])(implicit f: Field[F]): Identity[F] = Identity()
}
