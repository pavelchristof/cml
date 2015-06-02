package cml.models

import cml.Model
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

final case class AffineMap[In[_], Out[_]] (implicit
  inSpace: Cartesian[In],
  outSpace: Cartesian[Out]
) extends Model[In, Out] {
  override type Type[A] = Out[(In[A], A)]

  override implicit val space =
    Cartesian.compose[Out, ({type T[A] = (In[A], A)})#T](
      outSpace, Cartesian.product[In, ({type T[A] = A})#T](inSpace, Cartesian.Scalar))

  override def apply[F](inst: Type[F])(input: In[F])(implicit a: Analytic[F]): Out[F] = {
    import a.analyticSyntax._
    import ZeroFunctor.asZero
    outSpace.map(inst)(v => inSpace.dot(input, v._1) + v._2)
  }

  override def applySubspace[A](subspace: WholeSpace[Type], inst: Any)(input: In[A])
      (implicit a: Analytic[A]): Out[A] =
    apply(inst.asInstanceOf[subspace.Type[A]])(input)
}
