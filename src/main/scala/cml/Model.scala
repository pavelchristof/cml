package cml

import cml.algebra.traits._

import scala.reflect.ClassTag
import scala.util.Random
import scalaz.Functor

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
   * Creates a new model instance and fills it with some value. Even the infinite vector spaces will somehow be filled.
   * @param x Value the model parameters will be initialized with.
   * @tparam A The number type.
   */
  def fill[A](x: => A)(implicit a: Additive[A]): Type[A]

  /**
   * Initializes the model with small, random real numbers to break symmetry.
   */
  def symmetryBreaking[A](random: Random)(implicit a: Analytic[A]): Type[A] =
    fill(a.fromDouble((random.nextDouble() * 2) - 1))

  /**
   * Scores the data set.
   */
  def score[A](inst: Type[A])(data: Seq[(In[A], Out[A])])(implicit an: Analytic[A]): Seq[Sample[In[A], Out[A]]] =
    data.map{ case (in, out) => Sample(
      input = in,
      expected = out,
      actual = apply(inst)(in)
    )}
}

