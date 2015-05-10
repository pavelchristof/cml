package cml.models

import cml.algebra.traits._

/**
 * A machine learning model, taking some input and transforming it to some output.
 * @tparam In The input type, parametrized by the numeric type.
 * @tparam Out The output type, parametrized by the numeric type.
 */
trait Model[-In[_], +Out[_]] {
  /**
   * The type of model instances.
   */
  type Type[_] <: Serializable

  /**
   * Model instance is required to be a concrete vector space.
   */
  implicit val concrete: Concrete[Type]

  /**
   * Applies the model to some input.
   * @param input The input.
   * @param model The model instance.
   * @param f Numeric operations.
   * @tparam F The numeric type.
   * @return The output.
   */
  def apply[F](input: In[F])(model: Type[F])(implicit f: Analytic[F]): Out[F]

  /**
   * Creates a new model instance filled with some value.
   * @param x Value the model parameters will be initialized with.
   * @tparam F The number type.
   */
  def fill[F](x: => F): Type[F] = concrete.point(x)
}
