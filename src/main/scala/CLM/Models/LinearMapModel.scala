package CLM.Models

import CML.Algebra.{Normed, Analytic, Field}

import scalaz.{Functor, Applicative}

case class LinearMapModel[In[_], Out[_], F] (
  m: Out[In[F]]
) extends Serializable

case class LinearMap[In_[_], Out_[_]] (
  implicit outApplicative: Applicative[Out_],
  inNormed: Normed[In_],
  inFunctor: Functor[In_]
) extends Model[({type M[f] = LinearMapModel[In_, Out_, f]})#M] {
  import outApplicative.applicativeSyntax._

  override type In[A] = In_[A]
  override type Out[A] = Out_[A]

  override def predict[F](input: In[F], model: LinearMapModel[In, Out, F])(implicit f: Analytic[F]): Out[F] =
    model.m.map(inNormed.dot(input, _))

  override def div[F](v: LinearMapModel[In, Out, F], a: F)(implicit f: Field[F]): LinearMapModel[In, Out, F] =
    LinearMapModel[In, Out, F](v.m.map(inNormed.div(_, a)))

  override def mull[F](a: F, v: LinearMapModel[In, Out, F])(implicit f: Field[F]): LinearMapModel[In, Out, F] =
    LinearMapModel[In, Out, F](v.m.map(inNormed.mull(a, _)))

  override def mulr[F](v: LinearMapModel[In, Out, F], a: F)(implicit f: Field[F]): LinearMapModel[In, Out, F] =
    LinearMapModel[In, Out, F](v.m.map(inNormed.mulr(_, a)))

  override def zero[F](implicit f: Field[F]): LinearMapModel[In, Out, F] =
    LinearMapModel[In, Out, F](outApplicative.point(inNormed.zero[F]))

  override def add[F](x: LinearMapModel[In, Out, F], y: LinearMapModel[In, Out, F])
      (implicit f: Field[F]): LinearMapModel[In, Out, F] =
    LinearMapModel[In, Out, F](^(x.m, y.m) (inNormed.add(_, _)))

  override def neg[F](x: LinearMapModel[In, Out, F])(implicit f: Field[F]): LinearMapModel[In, Out, F] =
    LinearMapModel[In, Out, F](x.m.map(inNormed.neg(_)))

  override def map[A, B](fa: LinearMapModel[In, Out, A])(f: (A) => B): LinearMapModel[In, Out, B] =
    LinearMapModel[In, Out, B](fa.m.map(inFunctor(_)(f)))
}
