package cml

import cml.algebra._
import org.apache.spark.rdd.RDD
import scala.collection.parallel.ParSeq

/**
 * Machine learning models expressible as a differentiable function, mapping some input to some output.
 *
 * @tparam In The input type, parametrized by the numeric type.
 * @tparam Out The output type, parametrized by the numeric type.
 */
trait Model[In[_], Out[_]] {
  /**
   * The type of model instances.
   */
  type Type[A]

  /**
   * Model instance is required to be a representable vector space.
   */
  implicit val space: Representable[Type]

  /**
   * Applies the model to some input.
   * @param input The input.
   * @param inst The model instance.
   * @param a Numeric operations.
   * @tparam A The numeric type.
   * @return The output.
   */
  def apply[A](inst: Type[A])(input: In[A])(implicit a: Analytic[A]): Out[A]

  def applySample[A](inst: Type[A])(sample: (In[A], Out[A]))(implicit a: Analytic[A]): Sample[In[A], Out[A]] =
    Sample(
      input = sample._1,
      expected = sample._2,
      actual = apply(inst)(sample._1)
    )

  def gradient[A](data: Seq[(In[A], Out[A])], costFun: CostFun[In, Out])(cartesian: Cartesian[Type])
      (implicit adEngine: ad.Engine, a: Floating[A], in: ZeroFunctor[In], out: ZeroFunctor[Out]): (Type[A]) => (Type[A]) = {
    import adEngine.zero
    val prepData = data.map(s => (in.map(s._1)(adEngine.constant(_)), out.map(s._2)(adEngine.constant(_))))
    adEngine.grad[A, Type]((inst, ctx) => {
      implicit val augAn = adEngine.analytic(a, ctx)
      prepData
        .map(applySample(inst)(_))
        .map(costFun.scoreSample[adEngine.Aug[A]](_))
        .reduce(augAn.add(_, _))
    })(a, cartesian)
  }

  def regGradient[A](costFun: CostFun[In, Out])(cartesian: Cartesian[Type])
      (implicit adEngine: ad.Engine, a: Analytic[A]): (Type[A]) => (Type[A]) = {
    adEngine.grad[A, Type]((inst, ctx) => {
      implicit val augAn = adEngine.analytic(a, ctx)
      costFun.regularization[Type, adEngine.Aug[A]](inst)(augAn, cartesian)
    })(a, cartesian)
  }

  def cost[A](data: Seq[(In[A], Out[A])], costFun: CostFun[In, Out])(implicit a: Floating[A]): (Type[A]) => A =
    inst => {
      data
        .map(applySample(inst)(_))
        .map(costFun.scoreSample(_))
        .reduce(a.add(_, _))
    }

  def reg[A](costFun: CostFun[In, Out])(normed: Normed[Type])(implicit a: Floating[A]): (Type[A]) => A =
    inst => costFun.regularization[Type, A](inst)(a, normed)

  def convertSample[A, B](s: (In[A], Out[A]))
      (implicit a: Floating[A], b: Analytic[B], inFunctor: ZeroFunctor[In], outFunctor: ZeroFunctor[Out]): (In[B], Out[B]) = {
    def convert(x: A): B = b.fromDouble(a.toDouble(x))
    (inFunctor.map(s._1)(convert), outFunctor.map(s._2)(convert))
  }

  def restrict[A](data: Seq[(In[A], Out[A])], costFun: CostFun[In, Out])
      (implicit a: Floating[A], inFunctor: ZeroFunctor[In], outFunctor: ZeroFunctor[Out]) = {
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
      (implicit a: Floating[A], inFunctor: ZeroFunctor[In], outFunctor: ZeroFunctor[Out]) = {
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

