package cml

import cml.algebra.{Zero, ZeroFunctor}

import scalaz._

sealed trait Tree[+A, +B] extends Serializable {
  val accum: A

  /**
   * Split node values from leaf values.
   */
  def split: (Tree[Unit, B], Tree[A, Unit])

  /**
   * Zip two trees with the same structure.
   */
  def zip[C, D](t: Tree[C, D]): Tree[(A, C), (B, D)]
}

case class Node[+A, +B] (
  left: Tree[A, B],
  accum: A,
  right: Tree[A, B]
) extends Tree[A, B] {
  override def split: (Tree[Unit, B], Tree[A, Unit]) = {
    val (l1, l2) = left.split
    val (r1, r2) = right.split
    (Node(l1, (), r1), Node(l2, accum, r2))
  }

  override def zip[C, D](t: Tree[C, D]): Tree[(A, C), (B, D)] = t match {
    case Node(l, v, r) => Node(left.zip(l), (accum, v), right.zip(r))
    case _ => throw new Error("Tree.zip: tree structures do not match")
  }
}

case class Leaf[+A, +B] (
  accum: A,
  value: B
) extends Tree[A, B] {
  override def split: (Tree[Unit, B], Tree[A, Unit]) =
    (Leaf((), value), Leaf(accum, ()))

  override def zip[C, D](t: Tree[C, D]): Tree[(A, C), (B, D)] = t match {
    case Leaf(a, v) => Leaf((accum, a), (value, v))
    case _ => throw new Error("Tree.zip: tree structures do not match")
  }
}

object Tree {
  implicit def accumsZero[V] = new ZeroFunctor[({type T[A] = Tree[A, V]})#T] {
    override def map[A, B](v: Tree[A, V])(h: (A) => B)(implicit az: Zero[A], bz: Zero[B]): Tree[B, V] =
      v match {
        case Leaf(a, v) => Leaf(h(a), v)
        case Node(l, a, r) => Node(map(l)(h), h(a), map(r)(h))
      }
  }

  implicit def valuesZero[V] = new ZeroFunctor[({type T[A] = Tree[V, A]})#T] {
    override def map[A, B](v: Tree[V, A])(h: (A) => B)(implicit az: Zero[A], bz: Zero[B]): Tree[V, B] =
      v match {
        case Leaf(a, v) => Leaf(a, h(v))
        case Node(l, a, r) => Node(map(l)(h), a, map(r)(h))
      }
  }

  implicit def accums[V] = new Traverse1[({type T[A] = Tree[A, V]})#T] with Serializable {
    override def traverse1Impl[G[_], A, B](fa: Tree[A, V])(f: (A) => G[B])
        (implicit g: Apply[G]): G[Tree[B, V]] =
      fa match {
        case Leaf(a, v) => g.map(f(a))(Leaf(_, v))
        case Node(l, a, r) => {
          val ml = traverse1Impl(l)(f)
          val mr = traverse1Impl(r)(f)
          g.apply3(ml, f(a), mr)(Node(_, _, _))
        }
      }

    override def foldMapRight1[A, B](fa: Tree[A, V])(z: (A) => B)(f: (A, => B) => B): B =
      fa match {
        case Leaf(a, _) => z(a)
        case Node(l, a, r) => foldRight(l, f(a, foldMapRight1(r)(z)(f)))(f)
      }
  }

  implicit def values[V] = new Traverse1[({type T[A] = Tree[V, A]})#T] with Serializable {
    override def traverse1Impl[G[_], A, B](fa: Tree[V, A])(f: (A) => G[B])
        (implicit g: Apply[G]): G[Tree[V, B]] =
      fa match {
        case Leaf(a, v) => g.map(f(v))(Leaf(a, _))
        case Node(l, a, r) => {
          val ml = traverse1Impl(l)(f)
          val mr = traverse1Impl(r)(f)
          g.apply2(ml, mr)(Node(_, a, _))
        }
      }

    override def foldMapRight1[A, B](fa: Tree[V, A])(z: (A) => B)(f: (A, => B) => B): B =
      fa match {
        case Leaf(_, v) => z(v)
        case Node(l, _, r) => foldRight(l, foldMapRight1(r)(z)(f))(f)
      }
  }

  implicit object Bitraverse extends Bitraverse[Tree] with Serializable {
    override def bitraverseImpl[G[_], A, B, C, D](fab: Tree[A, B])(f: (A) => G[C], g: (B) => G[D])
        (implicit ap: Applicative[G]): G[Tree[C, D]] =
      fab match {
        case Leaf(a, v) => ap.apply2(f(a), g(v))(Leaf(_, _))
        case Node(l, a, r) => {
          val ml = bitraverseImpl(l)(f, g)
          val mr = bitraverseImpl(r)(f, g)
          ap.apply3(ml, f(a), mr)(Node(_, _, _))
        }
      }
  }
}
