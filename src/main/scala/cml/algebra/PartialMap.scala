package cml.algebra

import java.util

import scala.reflect.ClassTag

case class PartialMap[K, V] (
  keys: Vector[K],
  hashes: Array[Int],
  values: Array[V],
  default: (K) => V
) extends Serializable

object PartialMap {
  case class CartesianInst[K] (
    allKeys: Vector[K],
    allHashes: Array[Int]
  ) (implicit
    ord: Ordering[K]
  ) extends Cartesian[({type T[A] = PartialMap[K, A]})#T] {
    type Key = K

    val key2int = allKeys.zipWithIndex.toMap
    val int2key = allKeys.zipWithIndex.map(_.swap).toMap

    override val dim: Int = allKeys.size

    override def intToKey(i: Int): K = int2key(i)
    override def keyToInt(k: K): Int = key2int(k)

    override def zero[A](implicit a: Zero[A]) =
      PartialMap(Vector.empty, Array.emptyIntArray, Array.empty, _ => a.zero)

    override def map[A, B](v: PartialMap[K, A])(h: (A) => B)(implicit a: ClassTag[A], b: ClassTag[B]): PartialMap[K, B] =
      PartialMap(v.keys, v.hashes, v.values.map(h), k => h(v.default(k)))

    override def apply2[A, B, C](x: PartialMap[K, A], y: PartialMap[K, B])(h: (A, B) => C)
        (implicit a: ClassTag[A], b: ClassTag[B], c: ClassTag[C]): PartialMap[K, C] = {
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
        } else if (x.hashes(i) < y.hashes(j) || (x.hashes(i) == y.hashes(j) && ord.lt(x.keys(i), y.keys(j)))) {
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

      PartialMap(keys.result(), hashes.result(), values.result(), k => h(x.default(k), y.default(k)))
    }

    override def index[A](v: PartialMap[K, A])(k: K)(implicit a: ClassTag[A]): A = {
      indexOf(v.keys, v.hashes, k, k.hashCode()) match {
        case Some(i) => v.values(i)
        case None => v.default(k)
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

    override def tabulate[A](v: (K) => A)(implicit a: ClassTag[A]): PartialMap[K, A] =
      PartialMap(allKeys, allHashes, allKeys.map(v).toArray, v)

    override def tabulatePartial[A](v: Map[K, A])(implicit a: Zero[A]): PartialMap[K, A] = {
      val (hashes, keys, values) = v
        .toSeq
        .map(kv => ((kv._1.hashCode(), kv._1), kv._2))
        .sortBy(_._1)
        .map(khv => (khv._1._1, khv._1._2, khv._2))
        .unzip3
      PartialMap(keys.toVector, hashes.toArray, values.toArray, _ => a.zero)
    }

    override def sum[A](v: PartialMap[K, A])(implicit a: Additive[A]): A = {
      var s = a.zero
      var i = 0
      while (i < v.values.length) {
        s = a.add(s, v.values(i))
        i += 1
      }
      s
    }

    override def restrict(keySet: => Set[K]) = new Subspace[({type T[A] = PartialMap[K, A]})#T] {
      override type Type[A] = PartialMap[K, A]

      val (allKeys: Vector[K], allHashes: Array[Int]) = {
        val (hashes, keys) = keySet
          .intersect(CartesianInst.this.allKeys.toSet)
          .map(k => (k.hashCode(), k)).toSeq.sorted.unzip

        (keys.toVector, hashes.toArray)
      }

      override def inject[A](v: PartialMap[K, A])(implicit a: Zero[A]): PartialMap[K, A] =
        PartialMap(v.keys, v.hashes, v.values, (k: Key) =>
          if (keySet.contains(k)) v.default(k) else a.zero)

      override def project[A](v: PartialMap[K, A])(implicit a: Zero[A]): PartialMap[K, A] = {
        val values = new Array[A](allKeys.size)
        var i = 0
        while (i < allKeys.size) {
          values(i) = index(v)(allKeys(i))
          i += 1
        }
        PartialMap(allKeys, allHashes, values, _ => a.zero)
      }

      override implicit val space: CartesianInst[K] = CartesianInst(allKeys, allHashes)
    }

    override def classTag[A](implicit a: ClassTag[A]): ClassTag[PartialMap[K, A]] =
      new ClassTag[PartialMap[K, A]] {
        override def runtimeClass: Class[_] = classOf[PartialMap[K, A]]
      }
  }
}
