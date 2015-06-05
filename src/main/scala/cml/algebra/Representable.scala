package cml.algebra

import scala.collection.immutable.HashMap
import scala.reflect.ClassTag

trait Representable[F[_]] extends Linear[F] with ZeroApplicative[F] {
  type Key

  /**
   * Extracts the coefficient at the given key.
   */
  def index[A](v: F[A])(k: Key)(implicit a: Zero[A]): A

  /**
   * Creates a vector with coordinates given by a function.
   */
  def tabulate[A](v: (Key) => A)(implicit a: Zero[A]): F[A]

  /**
   * Creates a new vector from a map. Coefficients for keys not in the map are zero.
   */
  def tabulatePartial[A](v: Map[Key, A])(implicit a: Zero[A]): F[A]

  /**
   * Lifts a value.
   */
  override def point[A](x: A)(implicit a: Zero[A]): F[A] = tabulate(_ => x)

  /**
   * Returns a finitely-dimensional subspace of F, spanned (at least) by the unit vectors with
   * ones at positions given by the passed key set.
   */
  def restrict(keys: => Set[Key]): Cartesian[F]

  /**
   * Finds out the set of keys that the given function is accessing.
   *
   * Because the function terminates and assuming the reflector is accessed only using it's analytic instance
   * (i.e. the function h should really be polymorphic in the number type) then the set of accessed keys is finite
   * and constant for all arguments.
   */
  def reflect(h: (F[Reflector[Key]]) => Reflector[Key]): Set[Key] =
    h(tabulate(Reflector(_))).extract()
}

object Representable {
  import ZeroEndofunctor.asZero

  abstract class ProductBase[F[_], G[_]] (implicit val f: Representable[F], val g: Representable[G])
    extends Linear.Product[F, G] with Representable[({type T[A] = (F[A], G[A])})#T] {
    override type Key = Either[f.Key, g.Key]

    override def index[A](v: (F[A], G[A]))(k: Key)(implicit a: Zero[A]): A = k match {
      case Left(i) => f.index(v._1)(i)
      case Right(i) => g.index(v._2)(i)
    }

    override def tabulate[A](v: (Key) => A)(implicit a: Zero[A]): (F[A], G[A]) =
      (f.tabulate(k => v(Left(k))), g.tabulate(k => v(Right(k))))

    override def tabulatePartial[A](v: Map[Key, A])(implicit a: Zero[A]): (F[A], G[A]) = {
      val lefts = for ((i, v) <- v; j <- i.left.toSeq) yield (j, v)
      val rights = for ((i, v) <- v; j <- i.right.toSeq) yield (j, v)
      (f.tabulatePartial(lefts.toMap), g.tabulatePartial(rights.toMap))
    }

    override def point[A](x: A)(implicit a: Zero[A]): (F[A], G[A]) =
      (f.point(x), g.point(x))
  }

  class Product[F[_], G[_]] (implicit override val f: Representable[F], override val g: Representable[G])
      extends ProductBase[F, G] {
    override def restrict(keys: => Set[Key]) =
      Cartesian.product(
        f.restrict(keys.flatMap(_.left.toSeq)),
        g.restrict(keys.flatMap(_.right.toSeq)))
  }

  implicit def product[F[_], G[_]](implicit f: Representable[F], g: Representable[G]) = new Product[F, G]

  abstract class ComposeBase[F[_], G[_]] (implicit val f: Representable[F], val g: Representable[G])
    extends Linear.Compose[F, G] with Representable[({type T[A] = F[G[A]]})#T] {
    override type Key = (f.Key, g.Key)

    override def index[A](v: F[G[A]])(k: Key)(implicit a: Zero[A]): A =
      g.index(f.index(v)(k._1))(k._2)

    override def tabulate[A](v: (Key) => A)(implicit a: Zero[A]): F[G[A]] =
      f.tabulate(i => g.tabulate(j => v((i, j))))

    override def tabulatePartial[A](v: Map[Key, A])(implicit a: Zero[A]): F[G[A]] = {
      val u: Map[f.Key, Map[g.Key, A]] = v
        .groupBy(_._1._1)
        .mapValues(_.map(kv => (kv._1._2, kv._2)))
      f.tabulatePartial(u.mapValues(g.tabulatePartial(_)))
    }

    override def point[A](x: A)(implicit a: Zero[A]): F[G[A]] =
      f.point(g.point(x))
  }

  class Compose[F[_], G[_]] (implicit override val f: Representable[F], override val g: Representable[G])
    extends ComposeBase[F, G] with Representable[({type T[A] = F[G[A]]})#T] {
    override def restrict(keys: => Set[Key]) =
      Cartesian.compose(f.restrict(keys.map(_._1)), g.restrict(keys.map(_._2)))
  }

  implicit def compose[F[_], G[_]](implicit f: Representable[F], g: Representable[G]) = new Compose[F, G]

  case class HashMapWithDefault[A, +B] (
    base: HashMap[A, B],
    d: (A) => B
  ) extends Map[A, B] with Serializable {
    override def +[B1 >: B](kv: (A, B1)): HashMapWithDefault[A, B1] =
      HashMapWithDefault(base + kv, d)

    override def -(key: A): HashMapWithDefault[A, B] =
      HashMapWithDefault(base - key, d)

    override def get(key: A): Option[B] =
      base.get(key)

    override def iterator: Iterator[(A, B)] =
      base.iterator

    override def withDefault[B1 >: B](d: (A) => B1): HashMapWithDefault[A, B1] =
      HashMapWithDefault(base, d)

    override def withDefaultValue[B1 >: B](d: B1): HashMapWithDefault[A, B1] =
      HashMapWithDefault(base, SerializableFunction.Constant(d))

    override def default(key: A): B =
      d(key)

    override def mapValues[C](f: (B) => C): HashMapWithDefault[A, C] =
      HashMapWithDefault(base.map(kv => (kv._1, f(kv._2))), SerializableFunction.Compose(f, d))

    def zipWith[C, R](that: HashMapWithDefault[A, C], f: (B, C) => R): HashMapWithDefault[A, R] = {
      val b = HashMap.newBuilder[A, R]
      for ((k, v) <- base) {
        b += k -> f(v, that.applyOrElse(k, that.default))
      }
      for ((k, v) <- that.base if !contains(k)) {
        b += k -> f(d(k), v)
      }
      HashMapWithDefault(b.result(), new SerializableFunction[A, R] {
        override def apply(k: A): R = f(d(k), that.d(k))
      })
    }
  }

  class HashMapInst[K : ClassTag] extends Representable[({type T[A] = HashMapWithDefault[K, A]})#T] {
    override type Key = K

    override def zero[A](implicit a: Zero[A]): HashMapWithDefault[K, A] =
      HashMapWithDefault(HashMap(), SerializableFunction.Constant(a.zero))

    override def map[A, B](v: HashMapWithDefault[K, A])(h: (A) => B)
        (implicit a: Zero[A], b: Zero[B]): HashMapWithDefault[K, B] =
      v.mapValues(h)

    override def apply2[A, B, C](x: HashMapWithDefault[K, A], y: HashMapWithDefault[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): HashMapWithDefault[K, C] =
      x.zipWith(y, h)

    override def tabulate[A](v: (K) => A)(implicit a: Zero[A]): HashMapWithDefault[K, A] =
      HashMapWithDefault(HashMap(), v)

    override def tabulatePartial[A](v: Map[K, A])(implicit a: Zero[A]): HashMapWithDefault[K, A] =
      HashMapWithDefault(HashMap(v.toSeq : _*), SerializableFunction.Constant(a.zero))

    override def index[A](v: HashMapWithDefault[K, A])(k: K)(implicit a: Zero[A]): A =
      v.applyOrElse(k, v.default)

    override def restrict(keys: => Set[K]) =
      new RestrHashMapInst(keys.toArray, keys.zipWithIndex.toMap)

    override def add[A](x: HashMapWithDefault[K, A], y: HashMapWithDefault[K, A])
        (implicit a: Additive[A]): HashMapWithDefault[K, A] =
      if (x.size <= y.size) {
        var r = y.base
        for ((k, v) <- x) {
          r += k -> a.add(y.applyOrElse(k, y.default), v)
        }
        val d1 = x.d
        val d2 = y.d
        HashMapWithDefault(r, new SerializableFunction[K, A] {
          override def apply(k: K): A = a.add(d1(k), d2(k))
        })
      } else {
        add(y, x)
      }

    override def sub[A](x: HashMapWithDefault[K, A], y: HashMapWithDefault[K, A])
        (implicit a: Additive[A]): HashMapWithDefault[K, A] =
      add(x, neg(y))
  }

  class RestrHashMapInst[K : ClassTag] (
    keys: Array[K],
    key2Int: Map[K, Int]
  ) extends Cartesian[({type T[A] = HashMapWithDefault[K, A]})#T] {
    type Key = K

    override val dim: Int = key2Int.size

    override def intToKey(i: Int): K = keys(i)

    override def keyToInt(k: K): Int = key2Int(k)

    override def zero[A](implicit a: Zero[A]): HashMapWithDefault[K, A] =
      HashMapWithDefault(HashMap(), SerializableFunction.Constant(a.zero))

    override def map[A, B](v: HashMapWithDefault[K, A])(h: (A) => B)
        (implicit a: Zero[A], b: Zero[B]): HashMapWithDefault[K, B] =
      v.mapValues(h)

    override def apply2[A, B, C](x: HashMapWithDefault[K, A], y: HashMapWithDefault[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): HashMapWithDefault[K, C] = {
      val b = HashMap.newBuilder[K, C]
      for (k <- keys) {
        b += k -> h(x.applyOrElse(k, x.default), y.applyOrElse(k, y.default))
      }
      HashMapWithDefault(b.result(), SerializableFunction.Constant(c.zero))
    }

    override def tabulate[A](v: (K) => A)(implicit a: Zero[A]): HashMapWithDefault[K, A] =
      HashMapWithDefault(HashMap(keys.map(k => (k, v(k))) : _*), SerializableFunction.Constant(a.zero))

    override def index[A](v: HashMapWithDefault[K, A])(k: K)(implicit a: Zero[A]): A =
      v.applyOrElse(k, v.default)

    override def restrict(keys: => Set[K]) =
      new RestrHashMapInst(keys.toArray, keys.zipWithIndex.toMap)

    override def sum[A](v: HashMapWithDefault[K, A])(implicit a: Additive[A]): A = {
      var s = a.zero
      for ((k, _) <- key2Int) {
        s = a.add(s, index(v)(k))
      }
      s
    }
  }

  implicit def hashMap[K : ClassTag] = new HashMapInst[K]
}
