package cml.models

import cml._
import cml.algebra._

final case class Chain2[In[_], Mid[_], Out[_]] (
  m1: Model[In, Mid],
  m2: Model[Mid, Out]
) extends Model[In, Out] {
  override type Type[A] = (m1.Type[A], m2.Type[A])

  override implicit val space = Representable.product(m1.space, m2.space)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] =
    m2(inst._2)(m1(inst._1)(input))
}

final case class Chain3[In[_], Mid1[_], Mid2[_], Out[_]] (
  m1: Model[In, Mid1],
  m2: Model[Mid1, Mid2],
  m3: Model[Mid2, Out]
) extends Model[In, Out] {
  val chain = Chain2(m2, m3)

  override type Type[A] = (m1.Type[A], chain.Type[A])

  override implicit val space = Representable.product[m1.Type, chain.Type](m1.space, chain.space)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] =
    chain(inst._2)(m1(inst._1)(input))
}

final case class Chain4[In[_], Mid1[_], Mid2[_], Mid3[_], Out[_]] (
  m1: Model[In, Mid1],
  m2: Model[Mid1, Mid2],
  m3: Model[Mid2, Mid3],
  m4: Model[Mid3, Out]
) extends Model[In, Out] {
  val chain = Chain3(m2, m3, m4)

  override type Type[A] = (m1.Type[A], chain.Type[A])

  override implicit val space = Representable.product[m1.Type, chain.Type](m1.space, chain.space)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] =
    chain(inst._2)(m1(inst._1)(input))
}

final case class Chain5[In[_], Mid1[_], Mid2[_], Mid3[_], Mid4[_], Out[_]] (
  m1: Model[In, Mid1],
  m2: Model[Mid1, Mid2],
  m3: Model[Mid2, Mid3],
  m4: Model[Mid3, Mid4],
  m5: Model[Mid4, Out]
) extends Model[In, Out] {
  val chain = Chain4(m2, m3, m4, m5)

  override type Type[A] = (m1.Type[A], chain.Type[A])

  override implicit val space = Representable.product[m1.Type, chain.Type](m1.space, chain.space)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] =
    chain(inst._2)(m1(inst._1)(input))
}

final case class Chain6[In[_], Mid1[_], Mid2[_], Mid3[_], Mid4[_], Mid5[_], Out[_]] (
  m1: Model[In, Mid1],
  m2: Model[Mid1, Mid2],
  m3: Model[Mid2, Mid3],
  m4: Model[Mid3, Mid4],
  m5: Model[Mid4, Mid5],
  m6: Model[Mid5, Out]
) extends Model[In, Out] {
  val chain = Chain5(m2, m3, m4, m5, m6)

  override type Type[A] = (m1.Type[A], chain.Type[A])

  override implicit val space = Representable.product[m1.Type, chain.Type](m1.space, chain.space)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] =
    chain(inst._2)(m1(inst._1)(input))
}

final case class Chain7[In[_], Mid1[_], Mid2[_], Mid3[_], Mid4[_], Mid5[_], Mid6[_], Out[_]] (
  m1: Model[In, Mid1],
  m2: Model[Mid1, Mid2],
  m3: Model[Mid2, Mid3],
  m4: Model[Mid3, Mid4],
  m5: Model[Mid4, Mid5],
  m6: Model[Mid5, Mid6],
  m7: Model[Mid6, Out]
) extends Model[In, Out] {
  val chain = Chain6(m2, m3, m4, m5, m6, m7)

  override type Type[A] = (m1.Type[A], chain.Type[A])

  override implicit val space = Representable.product[m1.Type, chain.Type](m1.space, chain.space)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] =
    chain(inst._2)(m1(inst._1)(input))
}
