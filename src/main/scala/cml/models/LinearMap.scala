package cml.models

import cml.Model
import cml.algebra.Subspace.WholeSpace
import cml.algebra._

case class LinearMap[In[_], Out[_]] (
  implicit
  inSpace: Cartesian[In],
  outSpace: Cartesian[Out]
) extends Model[In, Out] {
  override type Params[A] = Out[In[A]]

  override implicit val params = Cartesian.compose[Out, In](outSpace, inSpace)

  import ClassTag1.asClassTag

  override def apply[A](inst: Params[A])(input: In[A])(implicit a: Analytic[A]): Out[A] =
    outSpace.map(inst)(inSpace.dot(input, _))
}
