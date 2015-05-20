package cml.models

import cml.Model
import cml.algebra.{Scalar, Compose, Product}
import cml.algebra.traits._

case class AffineMap[In[_], Out[_]] (
  implicit inLC: Concrete[In],
  outLC: Concrete[Out]
) extends Model[In, Out] {
  val biased = Product[In, Scalar.Type]()(inLC, Scalar)

  override type Type[A] = Out[biased.Type[A]]

  override implicit val space = Compose[Out, biased.Type]
    .locallyConcrete(outLC, biased)

  override def apply[F](inst: Type[F])(input: In[F])(implicit f: Analytic[F]): Out[F] = {
    import f.analyticSyntax._
    outLC.map(inst)(v => inLC.dot(input, v._1) + v._2)
  }

  override def fill[F](x: => F)(implicit a: Additive[F]): Type[F] =
    outLC.point((inLC.point(x), x))
}
