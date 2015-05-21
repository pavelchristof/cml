package cml.optimization

import cml.algebra.traits._

case class AdaGrad[V[_], A] (
  implicit an: Analytic[A],
  space: LocallyConcrete[V]
) extends GradTransformer[V, A] {
  var history: V[A] = space.zero
  override def apply(grad: V[A]): V[A] = {
    import an.analyticSyntax._
    val eps = fromDouble(1e-6)

    history = space.add(history, space.mapLC(grad)(_.square))
    space.apply2LC(grad, history){ case (x, y) => x / (y + eps).sqrt }
  }
}
