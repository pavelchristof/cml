package CLM.Models

import CML.Algebra.{Analytic, Field}

case class Compose[M1[_], M2[_], F] (
  val _1: M1[F],
  val _2: M2[F]
) extends Serializable

class ComposeModel[M1[_] <: Serializable, M2[_] <: Serializable, In[_], Mid[_], Out[_]] (
  implicit m1: Model[M1, In, Mid], m2: Model[M2, Mid, Out]
) extends Model[({type M[f] = Compose[M1, M2, f]})#M, In, Out] {
  override def predict[F](input: In[F], model: Compose[M1, M2, F])(implicit f: Analytic[F]): Out[F] =
    m2.predict(m1.predict(input, model._1), model._2)

  override def div[F](v: Compose[M1, M2, F], a: F)(implicit f: Field[F]): Compose[M1, M2, F] =
    Compose[M1, M2, F](m1.div(v._1, a), m2.div(v._2, a))

  override def mull[F](a: F, v: Compose[M1, M2, F])(implicit f: Field[F]): Compose[M1, M2, F] =
    Compose[M1, M2, F](m1.mull(a, v._1), m2.mull(a, v._2))

  override def mulr[F](v: Compose[M1, M2, F], a: F)(implicit f: Field[F]): Compose[M1, M2, F] =
    Compose[M1, M2, F](m1.mulr(v._1, a), m2.mulr(v._2, a))

  override def zero[F](implicit f: Field[F]): Compose[M1, M2, F] =
    Compose[M1, M2, F](m1.zero, m2.zero)

  override def add[F](x: Compose[M1, M2, F], y: Compose[M1, M2, F])(implicit f: Field[F]): Compose[M1, M2, F] =
    Compose[M1, M2, F](m1.add(x._1, y._1), m2.add(x._2, y._2))

  override def neg[F](x: Compose[M1, M2, F])(implicit f: Field[F]): Compose[M1, M2, F] =
    Compose[M1, M2, F](m1.neg(x._1), m2.neg(x._2))
}
