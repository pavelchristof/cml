package cml

import cml.algebra._

/**
 * Machine learning models expressible as a differentiable function, mapping some input to some output.
 *
 * @tparam In The input type, parametrized by the numeric type.
 * @tparam Out The output type, parametrized by the numeric type.
 */
trait Model[In[_], Out[_]] extends Serializable {
  /**
   * The space of parameters.
   */
  type Params[A]

  /**
   * Parameters must form a representable vector space.
   */
  implicit val params: Representable[Params]

  /**
   * Applies the model to some input.
   * @param input The input.
   * @param params The model parameters.
   * @param a Number operations (like addition, multiplication, trigonometric functions).
   * @tparam A The number type.
   * @return The output.
   */
  def apply[A](params: Params[A])(input: In[A])(implicit a: Analytic[A]): Out[A]

  def applySample[A](params: Params[A])(sample: (In[A], Out[A]))(implicit a: Analytic[A]): Sample[In[A], Out[A]] =
    Sample(
      input = sample._1,
      expected = sample._2,
      actual = apply(params)(sample._1)
    )

  def cost[A](data: Seq[(In[A], Out[A])], costFun: CostFun[In, Out])(implicit a: Floating[A]): (Params[A]) => A =
    inst => {
      data
        .map(applySample(inst)(_))
        .map(costFun.scoreSample(_))
        .reduce(a.add(_, _))
    }

  def reg[A](costFun: CostFun[In, Out])(normed: Normed[Params])(implicit a: Floating[A]): (Params[A]) => A =
    inst => costFun.regularization[Params, A](inst)(a, normed)

  def convertSample[A, B](s: (In[A], Out[A]))
      (implicit a: Floating[A], b: Analytic[B], inFunctor: Functor[In], outFunctor: Functor[Out]): (In[B], Out[B]) = {
    def convert(x: A): B = b.fromDouble(a.toDouble(x))
    (inFunctor.map(s._1)(convert), outFunctor.map(s._2)(convert))
  }

  def restrict[A](data: Seq[(In[A], Out[A])], costFun: CostFun[In, Out])
      (implicit a: Floating[A], inFunctor: Functor[In], outFunctor: Functor[Out]): Subspace[Params] = {
    val keys = data
      .map(convertSample[A, Reflector[params.Key]])
      .map(sample => params.reflect(inst => costFun.scoreSample(Sample[In[Reflector[params.Key]], Out[Reflector[params.Key]]](
        input = sample._1,
        expected = sample._2,
        actual = apply(inst)(sample._1)
      ))))
      .reduce(_ ++ _)
    params.restrict(keys)
  }

}

trait ParameterlessModel[In[_], Out[_]] extends Model[In, Out] {
  final override type Params[A] = Unit
  final override val params = Cartesian.Zero
  final override def apply[A](inst: Unit)(in: In[A])(implicit a: Analytic[A]): Out[A] = apply(in)

  def apply[A](in: In[A])(implicit a: Analytic[A]): Out[A]
}
