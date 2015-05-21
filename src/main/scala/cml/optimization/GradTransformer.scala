package cml.optimization

trait GradTransformer[V[_], A] {
  def apply(grad: V[A]): V[A]

  final def andThen(tr: GradTransformer[V, A]): GradTransformer[V, A] =
    new GradTransformer[V, A] {
      override def apply(grad: V[A]): V[A] = GradTransformer.this(tr(grad))
    }
}
