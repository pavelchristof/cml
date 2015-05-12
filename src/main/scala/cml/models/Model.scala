package cml.models

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
  type Type[_] <: Serializable

  /**
   * Model instance is required to be a locally concrete vector space.
   */
  implicit val locallyConcrete: LocallyConcrete[Type]

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
   * Creates a new model instance and fills it with some value. Only the concrete parts of the model will be
   * filled, the locally concrete vector spaces (like Maps) will be left with zeros.
   * @param x Value the model parameters will be initialized with.
   * @tparam F The number type.
   */
  def fill[F](x: => F)(implicit a: Additive[F]): Type[F]
}