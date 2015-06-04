package cml.models

import cml.algebra.Subspace.Product
import cml.algebra.{Analytic, Representable}
import cml._

final case class AccumulateTree[F[_], G[_]] (
  inject: Model[F, G],
  reduce: Model[({type T[A] = (G[A], G[A])})#T, G]
) extends Model[({type T[A] = Tree[Unit, F[A]]})#T, ({type T[A] = Tree[G[A], F[A]]})#T] {
  override type Type[A] = (inject.Type[A], reduce.Type[A])

  override implicit val space = Representable.product(inject.space, reduce.space)

  override def apply[A](inst: Type[A])(input: Tree[Unit, F[A]])(implicit a: Analytic[A]): Tree[G[A], F[A]] =
    input match {
      case Leaf(_, v) => Leaf(inject(inst._1)(v), v)
      case Node(l, _, r) => {
        val nl = apply(inst)(l)
        val nr = apply(inst)(r)
        Node(nl, reduce(inst._2)((nl.accum, nr.accum)), nr)
      }
    }

  override def applySubspace[A](
    subspace: Product[inject.Type, reduce.Type, space.f.AllowedSubspace, space.g.AllowedSubspace], instUntyped: Any)
      (input: Tree[Unit, F[A]])(implicit a: Analytic[A]): Tree[G[A], F[A]] = {
    val inst = instUntyped.asInstanceOf[subspace.Type[A]]
    val injSubsp = subspace._1.asInstanceOf[inject.space.AllowedSubspace]
    val redSubsp = subspace._2.asInstanceOf[reduce.space.AllowedSubspace]

    input match {
      case Leaf(_, v) => Leaf(inject.applySubspace(injSubsp, inst._1)(v), v)
      case Node(l, _, r) => {
        val nl = applySubspace(subspace, instUntyped)(l)
        val nr = applySubspace(subspace, instUntyped)(r)
        Node(nl, reduce.applySubspace(redSubsp, inst._2)((nl.accum, nr.accum)), nr)
      }
    }
  }
}
