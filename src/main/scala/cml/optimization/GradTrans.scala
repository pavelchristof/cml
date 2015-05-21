package cml.optimization

trait GradTrans[V[_], A] {
  def apply(grad: V[A]): V[A]

  final def andThen(tr: GradTrans[V, A]): GradTrans[V, A] =
    new GradTrans[V, A] {
      override def apply(grad: V[A]): V[A] = GradTrans.this(tr(grad))
    }
}
