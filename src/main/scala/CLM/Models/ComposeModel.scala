package CLM.Models

import CML.Algebra.{Analytic, Field}

case class ComposeModel[M1[_], M2[_], F] (
  val _1: M1[F],
  val _2: M2[F]
) extends Serializable

case class Compose[M1[_] <: Serializable, M2[_] <: Serializable] (
  implicit m1: Model[M1], m2: Model[M2]
) extends Model[({type M[f] = ComposeModel[M1, M2, f]})#M] {
  override type In[A] = m1.In[A]
  override type Out[A] = m2.Out[A]

  override def predict[F](input: In[F], model: ComposeModel[M1, M2, F])(implicit f: Analytic[F], e: Cons[F]): Out[F] =
    m2.predict(e._1(m1.predict(input, model._1))(f, e._2), model._2)(f, e._3)

  override def div[F](v: ComposeModel[M1, M2, F], a: F)(implicit f: Field[F]): ComposeModel[M1, M2, F] =
    ComposeModel[M1, M2, F](m1.div(v._1, a), m2.div(v._2, a))

  override def mull[F](a: F, v: ComposeModel[M1, M2, F])(implicit f: Field[F]): ComposeModel[M1, M2, F] =
    ComposeModel[M1, M2, F](m1.mull(a, v._1), m2.mull(a, v._2))

  override def mulr[F](v: ComposeModel[M1, M2, F], a: F)(implicit f: Field[F]): ComposeModel[M1, M2, F] =
    ComposeModel[M1, M2, F](m1.mulr(v._1, a), m2.mulr(v._2, a))

  override def zero[F](implicit f: Field[F]): ComposeModel[M1, M2, F] =
    ComposeModel[M1, M2, F](m1.zero, m2.zero)

  override def add[F](x: ComposeModel[M1, M2, F], y: ComposeModel[M1, M2, F])(implicit f: Field[F]): ComposeModel[M1, M2, F] =
    ComposeModel[M1, M2, F](m1.add(x._1, y._1), m2.add(x._2, y._2))

  override def neg[F](x: ComposeModel[M1, M2, F])(implicit f: Field[F]): ComposeModel[M1, M2, F] =
    ComposeModel[M1, M2, F](m1.neg(x._1), m2.neg(x._2))

  override def map[A, B](fa: ComposeModel[M1, M2, A])(f: (A) => B): ComposeModel[M1, M2, B] =
    ComposeModel[M1, M2, B](m1.map(fa._1)(f), m2.map(fa._2)(f))
}
