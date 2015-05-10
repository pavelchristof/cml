package cml.models

import cml.algebra.traits._
import cml.algebra

import scalaz.Traverse

case class Chain[-In[_], Mid[_], +Out[_]] (
  m1: Model[In, Mid],
  m2: Model[Mid, Out]
) extends Model[In, Out] {
  override type Type[A] = (m1.Type[A], m2.Type[A])

  override implicit val traverse = algebra.Product.traverse[m1.Type, m2.Type](m1.traverse, m2.traverse)
  override implicit val linear = algebra.Product.linear[m1.Type, m2.Type](m1.linear, m2.linear)

  override def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F] =
    m2(m1.apply[F](input)(model._1))(model._2)
}
