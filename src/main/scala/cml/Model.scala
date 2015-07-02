package cml

import cml.algebra._
import org.apache.spark.rdd.RDD

trait Category[Hom[_, _]] {
  def identity[A]: Hom[A, A]
  def compose[A, B, C](f: Hom[B, C], g: Hom[A, B]): Hom[A, C]
}

/**
 * Machine learning models expressible as a differentiable function, mapping some input to some output.
 *
 * @tparam In The input type, parametrized by the numeric type.
 * @tparam Out The output type, parametrized by the numeric type.
 */
trait Model[In[_], Out[_]] extends Serializable {
  /**
   * The type of model instances.
   */
  type Params[A]

  /**
   * Model instance is required to be a representable vector space.
   */
  implicit val space: Representable[Params]

  /**
   * Applies the model to some input.
   * @param input The input.
   * @param inst The model instance.
   * @param a Numeric operations.
   * @tparam A The numeric type.
   * @return The output.
   */
  def apply[A](inst: Params[A])(input: In[A])(implicit a: Analytic[A]): Out[A]

  def applySample[A](inst: Params[A])(sample: (In[A], Out[A]))(implicit a: Analytic[A]): Sample[In[A], Out[A]] =
    Sample(
      input = sample._1,
      expected = sample._2,
      actual = apply(inst)(sample._1)
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
      .map(convertSample[A, Reflector[space.Key]])
      .map(sample => space.reflect(inst => costFun.scoreSample(Sample[In[Reflector[space.Key]], Out[Reflector[space.Key]]](
        input = sample._1,
        expected = sample._2,
        actual = apply(inst)(sample._1)
      ))))
      .reduce(_ ++ _)
    space.restrict(keys)
  }

  def restrictRDD[A](data: RDD[(In[A], Out[A])], costFun: CostFun[In, Out])
      (implicit a: Floating[A], inFunctor: Functor[In], outFunctor: Functor[Out]) = {
    val keys = data
      .map(convertSample[A, Reflector[space.Key]])
      .map(sample => space.reflect(inst => costFun.scoreSample(Sample[In[Reflector[space.Key]], Out[Reflector[space.Key]]](
        input = sample._1,
        expected = sample._2,
        actual = apply(inst)(sample._1)
      ))))
      .reduce(_ ++ _)
    space.restrict(keys)
  }
}

trait ParameterlessModel[In[_], Out[_]] extends Model[In, Out] {
  final override type Params[A] = Unit
  final override val space = Cartesian.Zero
  final override def apply[A](inst: Unit)(in: In[A])(implicit a: Analytic[A]): Out[A] = apply(in)

  def apply[A](in: In[A])(implicit a: Analytic[A]): Out[A]
}
