package cml.optimization

import cml.algebra._

trait GradTrans extends Serializable {
  def create[V[_], A]()(implicit fl: Floating[A], space: Representable[V]): (V[A]) => V[A]

  def andThen(tr: GradTrans): GradTrans = new GradTrans {
    override def create[V[_], A]()(implicit fl: Floating[A], space: Representable[V]): (V[A]) => V[A] = {
      val f = GradTrans.this.create[V, A]()
      val g = tr.create[V, A]()
      x => g(f(x))
    }
  }
}

object GradTrans {
  object Identity extends GradTrans {
    override def create[V[_], A]()(implicit fl: Floating[A], space: Representable[V]): (V[A]) => V[A] = identity
  }
}