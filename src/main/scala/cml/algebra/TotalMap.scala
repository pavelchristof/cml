package cml.algebra

import java.util

case class TotalMap[K, V] (
  keys: Vector[K],
  hashes: Array[Int],
  values: Array[V],
  default: (K) => V
) extends Serializable

object TotalMap {
  class RepresentableInst[K](implicit ord: Ordering[K]) extends Representable[({type T[A] = TotalMap[K, A]})#T] {
    override type Key = K

    override def zero[A](implicit a: Zero[A]): TotalMap[K, A] =
      TotalMap(Vector.empty, Array.empty, Array.empty, _ => a.zero)

    override def map[A, B](v: TotalMap[K, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): TotalMap[K, B] =
      TotalMap(v.keys, v.hashes, v.values.map(h), v.default.andThen(h))

    override def apply2[A, B, C](x: TotalMap[K, A], y: TotalMap[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): TotalMap[K, C] = {
      val keys = Vector.newBuilder[K]
      val hashes = Array.newBuilder[Int]
      val values = Array.newBuilder[C]

      val n = x.hashes.length
      val m = y.hashes.length

      keys.sizeHint(n.max(m))
      hashes.sizeHint(n.max(m))
      values.sizeHint(n.max(m))

      var i = 0
      var j = 0

      while (i < n && j < m) {
        if (x.hashes(i) == y.hashes(j) && ord.equiv(x.keys(i), y.keys(j))) {
          keys += x.keys(i)
          hashes += x.hashes(i)
          values += h(x.values(i), y.values(j))
          i += 1
          j += 1
        } else if (x.hashes(i) < y.hashes(j) || (x.hashes(i) == y.hashes(j) && ord.lt(x.keys(i), y.keys(i)))) {
          keys += x.keys(i)
          hashes += x.hashes(i)
          values += h(x.values(i), y.default(x.keys(i)))
          i += 1
        } else {
          keys += y.keys(j)
          hashes += y.hashes(j)
          values += h(x.default(y.keys(j)), y.values(j))
          j += 1
        }
      }

      while (i < n) {
        keys += x.keys(i)
        hashes += x.hashes(i)
        values += h(x.values(i), y.default(x.keys(i)))
        i += 1
      }

      while (j < m) {
        keys += y.keys(j)
        hashes += y.hashes(j)
        values += h(x.default(y.keys(j)), y.values(j))
        j += 1
      }

      TotalMap(keys.result(), hashes.result(), values.result(), k => h(x.default(k), y.default(k)))
    }

    override def tabulate[A](v: (K) => A)(implicit a: Zero[A]): TotalMap[K, A] =
      TotalMap(Vector.empty, Array.empty, Array.empty, v)

    override def tabulatePartial[A](v: Map[K, A])(implicit a: Zero[A]): TotalMap[K, A] = {
      val (keys, hashes, values) = v.map(kv => (kv._1, kv._1.hashCode(), kv._2)).toSeq.sortBy(_._2).unzip3
      TotalMap(keys.toVector, hashes.toArray, values.toArray, _ => a.zero)
    }

    override def index[A](v: TotalMap[K, A])(k: K)(implicit a: Zero[A]): A = {
      indexOf(v.keys, v.hashes, k) match {
        case Some(i) => v.values(i)
        case None => v.default(k)
      }
    }

    def indexOf(keys: Vector[K], hashes: Array[Int], key: K): Option[Int] = {
      val hash = key.hashCode()
      var i = util.Arrays.binarySearch(hashes, hash)
      if (i >= 0) {
        var found = false
        while (!found && i < hashes.length && hash == hashes(i)) {
          if (key == keys(i)) {
            found = true
          } else {
            i += 1
          }
        }
        if (found) {
          Some(i)
        } else {
          None
        }
      } else {
        None
      }
    }

    override def restrict(keySet: => Set[K]) = new Subspace[({type T[A] = TotalMap[K, A]})#T] {
      override type Type[A] = InsaneMap[K, A]

      val (allKeys, allHashes) = {
        val (keys, hashes) = keySet.map(k => (k, k.hashCode())).toSeq.sortBy(_._2).unzip
        (keys.toVector, hashes.toArray)
      }

      override def project[A](v: TotalMap[K, A])(implicit a: Zero[A]): InsaneMap[K, A] = {
        val keys = Vector.newBuilder[K]
        val hashes = Array.newBuilder[Int]
        val values = Array.newBuilder[A]
        val n = v.hashes.length

        keys.sizeHint(n)
        hashes.sizeHint(n)
        values.sizeHint(n)

        var i = 0
        while (i < n) {
          if (util.Arrays.binarySearch(allHashes, v.hashes(i)) >= 0) {
            keys += v.keys(i)
            hashes += v.hashes(i)
            values += v.values(i)
          }
          i += 1
        }

        InsaneMap(keys.result(), hashes.result(), values.result())
      }

      override def inject[A](v: InsaneMap[K, A])(implicit a: Zero[A]): TotalMap[K, A] = {
        TotalMap(v.keys, v.hashes, v.values, _ => a.zero)
      }

      override implicit val space: Cartesian[Type] =
        InsaneMap.CartesianInst(allKeys)
    }
  }

  def representable[K](implicit ord: Ordering[K]) = new RepresentableInst[K]()
}