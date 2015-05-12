package cml.models

import cml.algebra.traits.{Additive, Analytic}

object Tree {
  def foldTree[A](t: Tree[A], f: ((A, A)) => A): A = t match {
    case Leaf(w) => w
    case Node(l, r) => f((foldTree(l, f), foldTree(r, f)))
  }
}

sealed abstract class Tree[T]
case class Node[T](l: Tree[T], r: Tree[T]) extends Tree[T]
case class Leaf[T](w: T) extends Tree[T]

case class FoldTree[In[_]] (
  m: Model[({type T[A] = (In[A], In[A])})#T, In]
) extends Model[({type T[A] = Tree[In[A]]})#T, In] {
  type Type[A] = m.Type[A]

  override implicit val locallyConcrete = m.locallyConcrete

  override def apply[F](input: Tree[In[F]])(model: this.Type[F])(implicit f: Analytic[F]): In[F] =
    Tree.foldTree(input, m(_: (In[F], In[F]))(model))

  def fill[F](x: => F)(implicit a: Additive[F]): Type[F] = m.fill(x)
}
