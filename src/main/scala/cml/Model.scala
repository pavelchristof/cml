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

  /**
   * Applies the model to some input using just a subspace of parameters (the rest are assumed to be 0).
   */
  def applySubspace[A](subspace: space.AllowedSubspace, inst: Any)(input: In[A])(implicit a: Analytic[A]): Out[A]

  /**
   * Applies the model to the data set.
   */
  def applySeq[A](inst: Type[A])(data: Seq[(In[A], Out[A])])(implicit a: Analytic[A]): Seq[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = apply(inst)(in)
    )}

  /**
   * Applies the model to the data set.
   */
  def applyParSeq[A](inst: Type[A])(data: ParSeq[(In[A], Out[A])])(implicit a: Analytic[A]): ParSeq[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = apply(inst)(in)
    )}

  /**
   * Applies the model to the data set.
   */
  def applyRDD[A](inst: Type[A])(data: RDD[(In[A], Out[A])])(implicit a: Analytic[A]): RDD[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = apply(inst)(in)
    )}

  /**
   * Applies the model to the data set.
   */
  def applySubspaceSeq[A](subspace: space.AllowedSubspace, inst: Any)(data: Seq[(In[A], Out[A])])
      (implicit a: Analytic[A]): Seq[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = applySubspace(subspace, inst)(in)
    )}

  def applySubspaceParSeq[A](subspace: space.AllowedSubspace, inst: Any)(data: ParSeq[(In[A], Out[A])])
      (implicit a: Analytic[A]): ParSeq[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = applySubspace(subspace, inst)(in)
    )}

  def applySubspaceRDD[A](subspace: space.AllowedSubspace, inst: Any)(data: RDD[(In[A], Out[A])])
      (implicit a: Analytic[A]): RDD[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = applySubspace(subspace, inst)(in)
    )}

  def convertSample[A, B](s: (In[A], Out[A]))
      (implicit a: Floating[A], b: Analytic[B], inFunctor: ZeroFunctor[In], outFunctor: ZeroFunctor[Out]): (In[B], Out[B]) = {
    def convert(x: A): B = b.fromDouble(a.toDouble(x))
    (inFunctor.map(s._1)(convert), outFunctor.map(s._2)(convert))
  }

  def restrict[A](data: RDD[(In[A], Out[A])], costFun: CostFun[In, Out])
      (implicit a: Floating[A], inFunctor: ZeroFunctor[In], outFunctor: ZeroFunctor[Out]): space.AllowedSubspace = {
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

