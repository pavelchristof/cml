package cml

import cml.algebra.traits._

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
   * Model instance is required to be a locally concrete vector space.
   */
  implicit val space: LocallyConcrete[Type]

  /**
   * Applies the model to some input.
   * @param input The input.
   * @param inst The model instance.
   * @param field Numeric operations.
   * @tparam A The numeric type.
   * @return The output.
   */
  def apply[A](inst: Type[A])(input: In[A])(implicit field: Analytic[A]): Out[A]

  /**
   * Applies the model to the data set.
   */
  def applySeq[A](inst: Type[A])(data: Seq[(In[A], Out[A])])(implicit an: Analytic[A]): Seq[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = apply(inst)(in)
    )}

  /**
   * Applies the model to the data set.
   */
  def applyParSeq[A](inst: Type[A])(data: ParSeq[(In[A], Out[A])])(implicit an: Analytic[A]): ParSeq[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = apply(inst)(in)
    )}
}

