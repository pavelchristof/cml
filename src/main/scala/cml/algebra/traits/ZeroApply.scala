package cml.algebra.traits

trait ZeroApply[V[_]] extends ZeroFunctor[V] {
  /**
   * Zips two "vectors".
   */
  def zip[A, B](x: V[A], y: V[B])(implicit a: Zero[A], b: Zero[B]): V[(A, B)]

  /**
   * Applies a function "pointwise".
   */
  def ap[A, B](x: V[A])(f: V[A => B])(implicit a: Zero[A], b: Zero[B]): V[B] =
    map(zip(x, f))(xf => xf._2(xf._1))

  /**
   *
   */
  def apply2[A, B, C](x: V[A], y: V[B])(f: (A, B) => C)(implicit a: Additive[A], b: Additive[B], c: Additive[C]): V[C]
}
