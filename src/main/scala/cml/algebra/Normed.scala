package cml.algebra

import scalaz.Scalaz._

/**
 * Vector spaces with norms and a bunch of similar stuff.
 */
trait Normed[F[_]] extends Representable[F] {
  def sum[A](v: F[A])(implicit a: Additive[A]): A

  final def taxicab[A](v: F[A])(implicit a: Analytic[A]): A
    = sum(map(v)(a.abs(_)))
  final def length[A](v: F[A])(implicit a: Analytic[A]): A
    = a.sqrt(quadrance(v))
  final def dist[A](u: F[A], v: F[A])(implicit a: Analytic[A]): A
    = length(sub(u, v))
  final def dot[A](u: F[A], v: F[A])(implicit a: AbelianRing[A]): A
    = sum(applyC2(u, v)(a.mul))
  final def quadrance[A](v: F[A])(implicit a: AbelianRing[A]): A
    = dot(v, v)
}

object Normed {
  import ZeroFunctor.asZero

  class Product[F[_], G[_]] (implicit override val f: Normed[F], override val g: Normed[G])
    extends Representable.Product[F, G] with Normed[({type T[A] = (F[A], G[A])})#T] {
    override def sum[A](v: (F[A], G[A]))(implicit a: Additive[A]): A =
      a.add(f.sum(v._1), g.sum(v._2))
  }

  implicit def product[F[_], G[_]](implicit f: Normed[F], g: Normed[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit override val f: Normed[F], override val g: Normed[G])
    extends Representable.Compose[F, G] with Normed[({type T[A] = F[G[A]]})#T] {
    override def sum[A](v: F[G[A]])(implicit a: Additive[A]): A =
      f.sum(f.map(v)(g.sum(_)))
  }

  implicit def compose[F[_], G[_]](implicit f: Normed[F], g: Normed[G]) = new Compose[F, G]

  class MapInst[K] extends Normed[({type T[A] = Map[K, A]})#T] {
    override type Key = K

    override def zero[A](implicit a: Zero[A]): Map[K, A] =
      Map()

    override def map[A, B](v: Map[K, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): Map[K, B] =
      v.mapValues(h)

    override def apply2[A, B, C](x: Map[K, A], y: Map[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Map[K, C] = {
      val allInX = x.map(kv => (kv._1, h(kv._2, y.getOrElse(kv._1, b.zero))))
      val inYButNotInX = y.flatMap(kv => if (x.contains(kv._1)) None else Some(kv._1, h(a.zero, kv._2)))
      allInX ++ inYButNotInX
    }

    override def applyC2[A, B, C](x: Map[K, A], y: Map[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Map[K, C] =
      x.intersectWith(y)(h)

    override def sum[A](v: Map[K, A])(implicit a: Additive[A]): A =
      v.values.fold(a.zero)(a.add)

    override def tabulate[A](v: Map[K, A])(implicit a: Zero[A]): Map[K, A] =
      v

    override def index[A](v: Map[K, A])(k: K)(implicit a: Zero[A]): A =
      v.getOrElse(k, a.zero)

    override def restrict(keys: Set[K]): Subspace[({type T[A] = Map[K, A]})#T] =
      new MapSubspace[K](keys.zipWithIndex.toMap)

    class MapSubspace[K] (keyMap: Map[K, Int]) extends Subspace[({type T[A] = Map[K, A]})#T] {
      val sizeNat = RuntimeNat(keyMap.size)

      override type Type[A] = Vec[sizeNat.Type, A]

      override implicit val space: Cartesian[Type] = Cartesian.vec(sizeNat())

      override def project[A](v: Map[K, A])(implicit a: Zero[A]): Type[A] = {
        val arr = new Array[A](keyMap.size)
        for ((k, i) <- keyMap) {
          arr(i) = v.getOrElse(k, a.zero)
        }
        Vec(arr)
      }

      override def inject[A](u: Type[A])(implicit a: Zero[A]): Map[K, A] = new Map[K, A] {
        override def +[B1 >: A](kv: (K, B1)): Map[K, B1] = throw new UnsupportedOperationException()
        override def -(key: K): Map[K, A] =  throw new UnsupportedOperationException()

        override def get(key: K): Option[A] = Some(u.get(keyMap(key)))

        override def iterator: Iterator[(K, A)] = keyMap.iterator.map(ki => (ki._1, u.get(ki._2)))
      }
    }
  }

  implicit def map[K] = new MapInst[K]
}
