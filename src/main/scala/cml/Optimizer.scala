package cml

import cml.algebra.ad
import cml.algebra.traits._

import scala.util.Random
import scalaz.Functor

trait Optimizer[In[_], Out[_]] {
  val model: Model[In, Out]
  def apply[A](
    population: Vector[model.Type[A]],
    data: Seq[(In[A], Out[A])],
    costFun: CostFun[In, Out]
  )(implicit
    an: Analytic[A],
    cmp: Ordering[A],
    diffEngine: ad.Engine
  ): Vector[model.Type[A]]
}
