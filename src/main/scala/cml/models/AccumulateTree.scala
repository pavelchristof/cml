package cml.models

import cml.algebra.Subspace.Product
import cml.algebra.{Analytic, Representable}
import cml._

final case class AccumulateTree[F[_], G[_]] (
  inject: Model[F, G],
  reduce: Model[({type T[A] = (G[A], G[A])})#T, G]
) extends Model[({type T[A] = Tree[Unit, F[A]]})#T, ({type T[A] = Tree[G[A], F[A]]})#T] {
  override type Params[A] = (inject.Params[A], reduce.Params[A])

  override implicit val space = Representable.product(inject.space, reduce.space)

  override def apply[A](inst: Params[A])(input: Tree[Unit, F[A]])(implicit a: Analytic[A]): Tree[G[A], F[A]] =
    input match {
      case Leaf(_, v) => Leaf(inject(inst._1)(v), v)
      case Node(l, _, r) => {
        val nl = apply(inst)(l)
        val nr = apply(inst)(r)
        Node(nl, reduce(inst._2)((nl.accum, nr.accum)), nr)
      }
    }
}
