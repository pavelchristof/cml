package cml.optimization

import cml.algebra.Floating
import cml.algebra.traits._

object AdaGrad extends GradTrans {
  override def create[V[_], A]()(implicit fl: Floating[A], space: Concrete[V]): (V[A]) => V[A] =
    new Function[V[A], V[A]] {
      var history: V[A] = space.zero
      override def apply(grad: V[A]): V[A] = {
        import fl.analyticSyntax._
        val eps = fromDouble(1e-6)

        history = space.add(history, space.map(grad)(_.square))
        space.apply2(grad, history){ case (x, y) => x / (y + eps).sqrt }
      }
  }
}
