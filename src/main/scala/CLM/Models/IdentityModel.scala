package CLM.Models

import CML.Algebra.{Analytic, Field}

case class IdentityModel[F] () extends Serializable

case class Identity[V[_]] () extends Model[IdentityModel] {
  override type In[A] = V[A]
  override type Out[A] = V[A]

  override def predict[F](input: V[F], model: Type[F])(implicit f: Analytic[F]): V[F] = input

  override def div[F](v: IdentityModel[F], a: F)(implicit f: Field[F]): IdentityModel[F] = IdentityModel()
  override def mull[F](a: F, v: IdentityModel[F])(implicit f: Field[F]): IdentityModel[F] = IdentityModel()
  override def mulr[F](v: IdentityModel[F], a: F)(implicit f: Field[F]): IdentityModel[F] = IdentityModel()
  override def zero[F](implicit f: Field[F]): IdentityModel[F] = IdentityModel()
  override def add[F](x: IdentityModel[F], y: IdentityModel[F])(implicit f: Field[F]): IdentityModel[F] = IdentityModel()
  override def neg[F](x: IdentityModel[F])(implicit f: Field[F]): IdentityModel[F] = IdentityModel()
  override def map[A, B](fa: IdentityModel[A])(f: (A) => B): IdentityModel[B] = IdentityModel()
}
