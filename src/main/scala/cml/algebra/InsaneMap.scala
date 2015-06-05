package cml.algebra

import java.util

case class InsaneMap[K, V] (
  hashes: Array[Int],
  values: Array[V]
) extends Serializable

object InsaneMap {
  case class CartesianInst[K] (
    allKeys: Vector[K]
  ) extends Cartesian[({type T[A] = InsaneMap[K, A]})#T] {
    type Key = K

    val key2int = allKeys.zipWithIndex.toMap
    val int2key = allKeys.zipWithIndex.map(_.swap).toMap
    val hashes = {
      val a = allKeys.map(_.hashCode()).toArray
      util.Arrays.sort(a)
      a
    }

    override val dim: Int = allKeys.size

    override def intToKey(i: Int): K = int2key(i)
    override def keyToInt(k: K): Int = key2int(k)

    override def zero[A](implicit a: Zero[A]) =
      InsaneMap(Array.emptyIntArray, Array.empty)

    override def map[A, B](v: InsaneMap[K, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): InsaneMap[K, B] =
      InsaneMap(v.hashes, v.values.map(h))

    override def apply2[A, B, C](x: InsaneMap[K, A], y: InsaneMap[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): InsaneMap[K, C] = {
      val hashes = Array.newBuilder[Int]
      val values = Array.newBuilder[C]

      val n = x.hashes.length
      val m = y.hashes.length

      hashes.sizeHint(n.max(m))
      values.sizeHint(n.max(m))

      var i = 0
      var j = 0

      while (i < n && j < m) {
        if (x.hashes(i) == y.hashes(j)) {
          hashes += x.hashes(i)
          values += h(x.values(i), y.values(j))
          i += 1
          j += 1
        } else if (x.hashes(i) < y.hashes(j)) {
          hashes += x.hashes(i)
          values += h(x.values(i), b.zero)
          i += 1
        } else {
          hashes += y.hashes(j)
          values += h(a.zero, y.values(j))
          j += 1
        }
      }

      while (i < n) {
        hashes += x.hashes(i)
        values += h(x.values(i), b.zero)
        i += 1
      }

      while (j < m) {
        hashes += y.hashes(j)
        values += h(a.zero, y.values(j))
        j += 1
      }

      InsaneMap(hashes.result(), values.result())
    }

    override def index[A](v: InsaneMap[K, A])(k: K)(implicit a: Zero[A]): A = {
      val i = util.Arrays.binarySearch(v.hashes, k.hashCode())
      if (i >= 0) {
        v.values(i)
      } else {
        a.zero
      }
    }

    override def tabulate[A](v: (K) => A)(implicit a: Zero[A]): InsaneMap[K, A] =
      InsaneMap(hashes, allKeys.map(v).toArray)

    override def sum[A](v: InsaneMap[K, A])(implicit a: Additive[A]): A = {
      var s = a.zero
      var i = 0
      while (i < v.values.size) {
        s = a.add(s, v.values(i))
        i += 1
      }
      s
    }

    override def restrict(keys: => Set[K]) = new Subspace[({type T[A] = InsaneMap[K, A]})#T] {
      override type Type[A] = InsaneMap[K, A]

      val keyArray = (keys ++ allKeys).toVector
      val allHashes = {
        val a = keyArray.map(_.hashCode()).toArray
        util.Arrays.sort(a)
        a
      }

      override def inject[A](v: InsaneMap[K, A])(implicit a: Zero[A]): InsaneMap[K, A] = v

      override def project[A](v: InsaneMap[K, A])(implicit a: Zero[A]): InsaneMap[K, A] = {
        val hashes = Array.newBuilder[Int]
        val values = Array.newBuilder[A]
        val n = v.hashes.length
        hashes.sizeHint(n)
        values.sizeHint(n)

        var i = 0
        while (i < n) {
          if (util.Arrays.binarySearch(allHashes, v.hashes(i)) >= 0) {
            hashes += v.hashes(i)
            values += v.values(i)
          }
          i += 1
        }

        InsaneMap(hashes.result(), values.result())
      }

      override implicit val space: Cartesian[Type] = CartesianInst(keyArray)
    }
  }
}
