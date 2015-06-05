package cml.algebra

import java.util

case class InsaneMap[K, V] (
  keys: Vector[K],
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
      InsaneMap(Vector.empty, Array.emptyIntArray, Array.empty)

    override def map[A, B](v: InsaneMap[K, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): InsaneMap[K, B] =
      InsaneMap(v.keys, v.hashes, v.values.map(h))

    override def apply2[A, B, C](x: InsaneMap[K, A], y: InsaneMap[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): InsaneMap[K, C] = {
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
        if (x.hashes(i) == y.hashes(j)) {
          keys += x.keys(i)
          hashes += x.hashes(i)
          values += h(x.values(i), y.values(j))
          i += 1
          j += 1
        } else if (x.hashes(i) < y.hashes(j)) {
          keys += x.keys(i)
          hashes += x.hashes(i)
          values += h(x.values(i), b.zero)
          i += 1
        } else {
          keys += y.keys(j)
          hashes += y.hashes(j)
          values += h(a.zero, y.values(j))
          j += 1
        }
      }

      while (i < n) {
        keys += x.keys(i)
        hashes += x.hashes(i)
        values += h(x.values(i), b.zero)
        i += 1
      }

      while (j < m) {
        keys += y.keys(j)
        hashes += y.hashes(j)
        values += h(a.zero, y.values(j))
        j += 1
      }

      InsaneMap(keys.result(), hashes.result(), values.result())
    }

    override def index[A](v: InsaneMap[K, A])(k: K)(implicit a: Zero[A]): A = {
      indexOf(v.keys, v.hashes, k, k.hashCode()) match {
        case Some(i) => v.values(i)
        case None => a.zero
      }
    }

    def indexOf(keys: Vector[K], hashes: Array[Int], key: K, hash: Int): Option[Int] = {
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

    override def tabulate[A](v: (K) => A)(implicit a: Zero[A]): InsaneMap[K, A] =
      InsaneMap(allKeys, hashes, allKeys.map(v).toArray)

    override def sum[A](v: InsaneMap[K, A])(implicit a: Additive[A]): A = {
      var s = a.zero
      var i = 0
      while (i < v.values.size) {
        s = a.add(s, v.values(i))
        i += 1
      }
      s
    }

    override def restrict(keySet: => Set[K]) = new Subspace[({type T[A] = InsaneMap[K, A]})#T] {
      override type Type[A] = InsaneMap[K, A]

      val (allKeys, allHashes) = {
        val (keys, hashes) = keySet.intersect(CartesianInst.this.allKeys.toSet)
          .map(k => (k, k.hashCode())).toSeq.sortBy(_._2).unzip
        (keys.toVector, hashes.toArray)
      }

      override def inject[A](v: InsaneMap[K, A])(implicit a: Zero[A]): InsaneMap[K, A] = v

      override def project[A](v: InsaneMap[K, A])(implicit a: Zero[A]): InsaneMap[K, A] = {
        val values = new Array[A](allKeys.size)
        var i = 0
        while (i < allKeys.size) {
          values(i) = indexOf(v.keys, v.hashes, allKeys(i), allHashes(i)) match {
            case Some(j) => v.values(j)
            case None => a.zero
          }
          i += 1
        }
        InsaneMap(allKeys, allHashes, values)
      }

      override implicit val space: Cartesian[Type] = CartesianInst(allKeys)
    }
  }
}
