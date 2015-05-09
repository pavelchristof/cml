package CLM.Models

import CML.Algebra.{Normed, Analytic, Field}

import scalaz.{Applicative}

case class LinearMap[In[_], Out[_], F] (
  m: Out[In[F]]
) extends Serializable

class LinearMapModel[In[_], Out[_]] (
  implicit outApplicative: Applicative[Out],
  inNormed: Normed[In]
) extends Model[({type M[f] = LinearMap[In, Out, f]})#M, In, Out] {
  import outApplicative.applicativeSyntax._

  override def predict[F](input: In[F], model: LinearMap[In, Out, F])(implicit f: Analytic[F]): Out[F] =
    model.m.map(inNormed.dot(input, _))

  override def div[F](v: LinearMap[In, Out, F], a: F)(implicit f: Field[F]): LinearMap[In, Out, F] =
    LinearMap[In, Out, F](v.m.map(inNormed.div(_, a)))

  override def mull[F](a: F, v: LinearMap[In, Out, F])(implicit f: Field[F]): LinearMap[In, Out, F] =
    LinearMap[In, Out, F](v.m.map(inNormed.mull(a, _)))

  override def mulr[F](v: LinearMap[In, Out, F], a: F)(implicit f: Field[F]): LinearMap[In, Out, F] =
    LinearMap[In, Out, F](v.m.map(inNormed.mulr(_, a)))

  override def zero[F](implicit f: Field[F]): LinearMap[In, Out, F] =
    LinearMap[In, Out, F](outApplicative.point(inNormed.zero[F]))

  override def add[F](x: LinearMap[In, Out, F], y: LinearMap[In, Out, F])
      (implicit f: Field[F]): LinearMap[In, Out, F] =
    LinearMap[In, Out, F](^(x.m, y.m) (inNormed.add(_, _)))

  override def neg[F](x: LinearMap[In, Out, F])(implicit f: Field[F]): LinearMap[In, Out, F] =
    LinearMap[In, Out, F](x.m.map(inNormed.neg(_)))
}
