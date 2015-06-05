package cml.algebra

import cml.algebra.Subspace.WholeSpace

import scala.collection.immutable.HashMap
import scala.reflect.ClassTag

sealed trait TotalMap[K, A] extends Serializable {
  def apply(k: K)(implicit a: Zero[A]): A
  def default(implicit a: Zero[A]): (K) => A
  def keySet: Set[K]
}

private case class DefaultMap[K, A] (
  d: (K) => A
) extends TotalMap[K, A] {
  override def apply(k: K)(implicit a: Zero[A]): A = d(k)
  override def default(implicit a: Zero[A]) = d
  override def keySet: Set[K] = Set()
}

private case class HashMapWithDefault[K, A] (
  hashMap: HashMap[K, A],
  d: (K) => A
) extends TotalMap[K, A] {
  override def apply(k: K)(implicit a: Zero[A]): A = hashMap.applyOrElse(k, default)
  override def default(implicit a: Zero[A]) = d
  override def keySet: Set[K] = hashMap.keySet
}

private case class ArrayMapWithDefault[K, A] (
  keys: Array[K],
  ind: HashMap[K, Int],
  array: Array[A],
  d: (K) => A
) extends TotalMap[K, A] {
  override def apply(k: K)(implicit a: Zero[A]): A = ind.get(k) match {
    case Some(i) => array(i)
    case None => d(k)
  }
  override def default(implicit a: Zero[A]) = d
  override def keySet: Set[K] = keys.toSet
}

/* private */ case class ArrayMap[K, A] (
  keys: Array[K],
  ind: HashMap[K, Int],
  array: Array[A]
) extends TotalMap[K, A] {
  override def apply(k: K)(implicit a: Zero[A]): A = ind.get(k) match {
    case Some(i) => array(i)
    case None => a.zero
  }
  override def default(implicit a: Zero[A]) = _ => a.zero
  override def keySet: Set[K] = keys.toSet
}

object TotalMap {
  implicit class RepresentableImpl[K] (classTag: ClassTag[K])
    extends Representable[({type T[+A] = TotalMap[K, A]})#T] {
    override type Key = K

    /**
     * Each object (A, zero : A) is mapped to an object (F[A], zero : F[A]).
     */
    override def zero[A](implicit a: Zero[A]): TotalMap[K, A] =
      DefaultMap(SerializableFunction.Constant(a.zero))

    /**
     * Lifts a zero preserving function.
     */
    override def map[A, B](v: TotalMap[K, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): TotalMap[K, B] =
      v match {
        case DefaultMap(d) => DefaultMap(d.andThen(h))
        case HashMapWithDefault(hashMap, default) =>
          HashMapWithDefault(hashMap.map(kv => kv._1 -> h(kv._2)), default.andThen(h))
        case ArrayMapWithDefault(keys, ind, array, default) =>
          ArrayMapWithDefault(keys, ind, array.map(h), default.andThen(h))
        case ArrayMap(keys, ind, array) =>
          ArrayMap(keys, ind, array.map(h))
      }

    /**
     * Zips two "vectors" and applies a function pointwise.
     */
    override def apply2[A, B, C](x: TotalMap[K, A], y: TotalMap[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): TotalMap[K, C] =
      (x, y) match {
        case (DefaultMap(d1), DefaultMap(d2)) =>
          DefaultMap(k => h(d1(k), d2(k)))

        case (DefaultMap(d1), ArrayMapWithDefault(k, i, ar, d2)) => {
          val ar2 = Array.tabulate(ar.size)(i => h(d1(k(i)), ar(i)))
          ArrayMapWithDefault(k, i, ar2, k => h(d1(k), d2(k)))
        }
        case (ArrayMapWithDefault(k, i, ar, d1), DefaultMap(d2)) => {
          val ar2 = Array.tabulate(ar.size)(i => h(ar(i), d2(k(i))))
          ArrayMapWithDefault(k, i, ar2, k => h(d1(k), d2(k)))
        }

        case (DefaultMap(d1), ArrayMap(k, i, ar)) => {
          val ar2 = Array.tabulate(ar.size)(i => h(d1(k(i)), ar(i)))
          ArrayMapWithDefault(k, i, ar2, k => h(d1(k), b.zero))
        }
        case (ArrayMap(k, i, ar), DefaultMap(d2)) => {
          val ar2 = Array.tabulate(ar.size)(i => h(ar(i), d2(k(i))))
          ArrayMapWithDefault(k, i, ar2, k => h(a.zero, d2(k)))
        }

        case _ => HashMapWithDefault(
          HashMap((x.keySet ++ y.keySet).map(k => k -> h(x(k), y(k))).toSeq : _*),
          k => h(x.default(a)(k), y.default(b)(k)))
      }

    /**
     * Extracts the coefficient at the given key.
     */
    override def index[A](v: TotalMap[K, A])(k: Key)(implicit a: Zero[A]): A =
      v(k)

    /**
     * Creates a vector with coordinates given by a function.
     */
    override def tabulate[A](v: (Key) => A)(implicit a: Zero[A]): TotalMap[K, A] =
      DefaultMap(v)

    /**
     * Creates a new vector from a map. Coefficients for keys not in the map are zero.
     */
    override def tabulatePartial[A](v: Map[Key, A])(implicit a: Zero[A]): TotalMap[K, A] =
      HashMapWithDefault(HashMap(v.toSeq : _*), _ => a.zero)

    /**
     * Returns a finitely-dimensional subspace of F, spanned (at least) by the unit vectors with
     * ones at positions given by the passed key set.
     */
    override def restrict(keys: => Set[Key]): Subspace[({type T[A] = TotalMap[K, A]})#T] =
      SubspaceImpl[K](keys.toArray(classTag), HashMap(keys.zipWithIndex.toSeq : _*))(classTag)
  }

  def representable[K](implicit classTag: ClassTag[K]) = new RepresentableImpl(classTag)

  private case class SubspaceImpl[K] (
    keys: Array[K],
    indices: HashMap[K, Int]
  ) (implicit
    classTag: ClassTag[K]
  ) extends Subspace[({type T[+A] = TotalMap[K, A]})#T] {
    override type Type[A] = ArrayMap[K, A]

    override def project[A](v: TotalMap[K, A])(implicit a: Zero[A]): Type[A] =
      ArrayMap(keys, indices, Array.tabulate(keys.size)(i => v(keys(i))))

    override def inject[A](v: ArrayMap[K, A])(implicit a: Zero[A]): TotalMap[K, A] = v

    override implicit val space: Cartesian[Type] = CartesianImpl(keys, indices)
  }

  private case class CartesianImpl[K] (
    keys: Array[K],
    indices: HashMap[K, Int]
  ) (implicit
    classTag: ClassTag[K]
  ) extends Cartesian[({type T[+A] = ArrayMap[K, A]})#T] {
    override type Key = K

    override val dim: Int = keys.size

    override def intToKey(i: Int): K = keys(i)

    override def keyToInt(k: K): Int = indices(k)

    override def zero[A](implicit a: Zero[A]): ArrayMap[K, A] =
      ArrayMap(keys, indices, Array.fill(keys.size)(a.zero))

    override def map[A, B](v: ArrayMap[K, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): ArrayMap[K, B] =
      ArrayMap(keys, indices, v.array.map(h))

    override def apply2[A, B, C](x: ArrayMap[K, A], y: ArrayMap[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): ArrayMap[K, C] =
      ArrayMap(keys, indices, Array.tabulate(keys.size)(i => h(x.array(i), y.array(i))))

    override def tabulate[A](v: K => A)(implicit a: Zero[A]): ArrayMap[K, A] =
      ArrayMap(keys, indices, keys.map(v))

    override def index[A](v: ArrayMap[K, A])(k: K)(implicit a: Zero[A]): A =
      v(k)

    override def sum[A](v: ArrayMap[K, A])(implicit a: Additive[A]): A = {
      var s = a.zero
      var i = 0
      while (i < v.array.size) {
        s = a.add(s, v.array(i))
        i += 1
      }
      s
    }

    override def restrict(keys: => Set[K]): Subspace[({type T[A] = ArrayMap[K, A]})#T] =
      new WholeSpace[({type T[+A] = ArrayMap[K, A]})#T]()(this)
  }
}
