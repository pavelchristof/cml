package cml.models

import cml.algebra.traits._
import scalaz.Functor

case class Chain[-In[_], Mid[_], +Out[_]] (
  m1: Model[In, Mid],
  m2: Model[Mid, Out]
) extends Model[In, Out] {
  override type Type[A] = (m1.Type[A], m2.Type[A])

  override implicit val functor: Functor[Type] = null
  override implicit val linear: Linear[Type] = null

  override def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F] =
    m2(m1.apply[F](input)(model._1))(model._2)
}
