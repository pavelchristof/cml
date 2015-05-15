package cml

import cml.algebra.traits._

/**
 * Machine learning models expressible as a differentiable function, mapping some input to some output.
 *
 * @tparam In The input type, parametrized by the numeric type.
 * @tparam Out The output type, parametrized by the numeric type.
 */
trait Model[-In[_], +Out[_]] {
  /**
   * The type of model instances.
   */
  type Type[_]

  /**
   * Model instance is required to be a locally concrete vector space.
   */
  implicit val locallyConcrete: LocallyConcrete[Type]

  /**
   * Applies the model to some input.
   * @param input The input.
   * @param model The model instance.
   * @param field Numeric operations.
   * @tparam A The numeric type.
   * @return The output.
   */
  def apply[A](input: In[A])(model: Type[A])(implicit field: Analytic[A]): Out[A]

  /**
   * Creates a new model instance and fills it with some value. Only the finite parts of the model will be
   * filled, the infinite vector spaces (like Maps) will be left with zeros.
   * @param x Value the model parameters will be initialized with.
   * @tparam A The number type.
   */
  def fill[A](x: => A)(implicit a: Additive[A]): Type[A]
}
