package cml.algebra

import scalaz.Scalaz._

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
  def restrict(keys: => Set[Key]): Subspace[F]

  /**
   * Finds out the set of keys that the given function is accessing.
   *
   * Because the function terminates and assuming the reflector is accessed only using it's analytic instance
   * (i.e. the function h should really be polymorphic in the number type) then the set of accessed keys is finite
   * and constant for all arguments.
   */
  def reflect(h: (F[Reflector[Key]]) => Reflector[Key]): Set[Key] =
    h(tabulate(k => Reflector(Set(k)))).keys
}

object Representable {
  import ZeroFunctor.asZero

  class Product[F[_], G[_]] (implicit val f: Representable[F], val g: Representable[G])
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

    override def restrict(keys: => Set[Key]): Subspace[({type T[A] = (F[A], G[A])})#T] =
      new Subspace.Product[F, G](
        f.restrict(keys.flatMap(_.left.toSeq)),
        g.restrict(keys.flatMap(_.right.toSeq)))
  }

  implicit def product[F[_], G[_]](implicit f: Representable[F], g: Representable[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit val f: Representable[F], val g: Representable[G])
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

    override def restrict(keys: => Set[Key]): Subspace[({type T[A] = F[G[A]]})#T] =
      new Subspace.Compose[F, G](f.restrict(keys.map(_._1)), g.restrict(keys.map(_._2)))
  }

  implicit def compose[F[_], G[_]](implicit f: Representable[F], g: Representable[G]) = new Compose[F, G]

  class MapInst[K] extends Representable[({type T[A] = Map[K, A]})#T] {
    override type Key = K

    override def zero[A](implicit a: Zero[A]): Map[K, A] =
      Map().withDefaultValue(a.zero)

    override def map[A, B](v: Map[K, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): Map[K, B] =
      v.mapValues(h).withDefault(k => h(v.default(k)))

    override def apply2[A, B, C](x: Map[K, A], y: Map[K, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Map[K, C] = {
      val allInX = x.map(kv => (kv._1, h(kv._2, y.applyOrElse(kv._1, y.default))))
      val inYButNotInX = y.flatMap(kv => if (x.contains(kv._1)) None else Some(kv._1, h(x.default(kv._1), kv._2)))
      (allInX ++ inYButNotInX).withDefault(k => h(x.default(k), y.default(k)))
    }

    override def tabulate[A](v: (K) => A)(implicit a: Zero[A]): Map[K, A] =
      Map().withDefault(v)

    override def tabulatePartial[A](v: Map[K, A])(implicit a: Zero[A]): Map[K, A] =
      v.withDefaultValue(a.zero)

    override def index[A](v: Map[K, A])(k: K)(implicit a: Zero[A]): A =
      v.applyOrElse(k, v.default)

    override def restrict(keys: => Set[K]): Subspace[({type T[A] = Map[K, A]})#T] =
      new MapSubspace[K](keys.zipWithIndex.toMap)

    class MapSubspace[K] (keyMap: Map[K, Int]) extends Subspace[({type T[A] = Map[K, A]})#T] {
      val sizeNat = RuntimeNat(keyMap.size)

      override type Type[A] = Vec[sizeNat.Type, A]

      override implicit val space: Cartesian[Type] = Cartesian.vec(sizeNat())

      override def project[A](v: Map[K, A])(implicit a: Zero[A]): Type[A] = {
        val arr = new Array[A](keyMap.size)
        for ((k, i) <- keyMap) {
          arr(i) = v.applyOrElse(k, v.default)
        }
        Vec(arr)
      }

      override def inject[A](u: Type[A])(implicit a: Zero[A]): Map[K, A] = new Map[K, A] {
        override def +[B1 >: A](kv: (K, B1)): Map[K, B1] = throw new UnsupportedOperationException()
        override def -(key: K): Map[K, A] =  throw new UnsupportedOperationException()

        override def get(key: K): Option[A] = Some(u.get(keyMap(key)))

        override def iterator: Iterator[(K, A)] = keyMap.iterator.map(ki => (ki._1, u.get(ki._2)))
      }.withDefaultValue(a.zero)
    }
  }

  implicit def map[K] = new MapInst[K]
}
