package cml.models

import cml.algebra.traits._

import scalaz.{Functor, Applicative}

case class LinearMap[In[_], Out[_]] (
  implicit outApplicative: Applicative[Out],
  inNormed: Normed[In],
  inFunctor: Functor[In]
) extends Model[In, Out] {
  override type Type[A] = Out[In[A]]

  import outApplicative.applicativeSyntax._

  override implicit val functor = new Functor[Type] {
    override def map[A, B](fa: Type[A])(f: (A) => B): Type[B] = outApplicative.map(fa)(inFunctor.map(_)(f))
  }

  override implicit val linear: Linear[Type] = new Linear[Type] {
    override def zero[F](implicit f: Field[F]): Type[F] = outApplicative.point(inNormed.zero)
    override def add[F](x: Type[F], y: Type[F])(implicit f: Field[F]): Type[F] = ^(x, y) (inNormed.add(_, _))
    override def neg[F](x: Type[F])(implicit f: Field[F]): Type[F] = outApplicative.map(x)(inNormed.neg(_))

    override def mull[F](a: F, v: Type[F])(implicit f: Field[F]): Type[F] = outApplicative.map(v)(inNormed.mull(a, _))
    override def mulr[F](v: Type[F], a: F)(implicit f: Field[F]): Type[F] = outApplicative.map(v)(inNormed.mulr(_, a))
    override def div[F](v: Type[F], a: F)(implicit f: Field[F]): Type[F] = outApplicative.map(v)(inNormed.div(_, a))
  }

  override def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F] =
    outApplicative.map(model)(inNormed.dot(input, _))
}
