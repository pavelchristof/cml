package cml.models

import cml.Model
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

case class LinearMap[In[_], Out[_]] (
  implicit
  inSpace: Cartesian[In],
  outSpace: Cartesian[Out]
) extends Model[In, Out] {
  override type Type[A] = Out[In[A]]

  override implicit val space = Cartesian.compose[Out, In](outSpace, inSpace)

  import ZeroFunctor.asZero

  override def apply[A](inst: Type[A])(input: In[A])(implicit a: Analytic[A]): Out[A] =
    outSpace.map(inst)(inSpace.dot(input, _))

  override def applySubspace[A](subspace: WholeSpace[Type], inst: Any)(input: In[A])(implicit a: Analytic[A]): Out[A] =
    apply(inst.asInstanceOf[subspace.Type[A]])(input)
}
