package cml.optimization

import cml.algebra._

trait GradTrans extends Serializable {
  def create[V[_], A]()(implicit fl: Floating[A], space: Cartesian[V]): (V[A]) => V[A]

  def andThen(tr: GradTrans): GradTrans = new GradTrans {
    override def create[V[_], A]()(implicit fl: Floating[A], space: Cartesian[V]): (V[A]) => V[A] = {
      val f = GradTrans.this.create[V, A]()
      val g = tr.create[V, A]()
      x => g(f(x))
    }
  }
}
