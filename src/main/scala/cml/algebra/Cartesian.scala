package cml.algebra

import cml.algebra.Subspace.WholeSpace

import scala.reflect.ClassTag

/**
 * A functor that maps field F to the cartesian space F&#94;n for some natural number n.
 */
trait Cartesian[F[_]] extends Normed[F] {
  /**
   * Dimension of this vector space.
   */
  val dim: Int

  /**
   * Bijection between keys and integers in range [0, dim-1].
   */
  def keyToInt(k: Key): Int

  /**
   * The inverse of keyToInt.
   */
  def intToKey(i: Int): Key

  /**
   * Creates a new vector from a map. Coefficients for keys not in the map are zero.
   */
  override def tabulatePartial[A](v: Map[Key, A])(implicit a: Zero[A]): F[A] =
    tabulate(k => v.getOrElse(k, a.zero))

  def fromSeq[A](v: Seq[A])(implicit a: ClassTag[A]): Option[F[A]] =
    if (v.size < dim)
      None
    else
      Some(tabulate(k => v(keyToInt(k))))
}

object Cartesian {
  import ClassTag1.asClassTag

  class Product[F[_], G[_]] (implicit override val f: Cartesian[F], override val g: Cartesian[G])
    extends Representable.Product[F, G] with Cartesian[({type T[A] = (F[A], G[A])})#T] {
    override val dim: Int = f.dim + g.dim

    override def keyToInt(k: Either[f.Key, g.Key]): Int = k match {
      case Left(i) => f.keyToInt(i)
      case Right(i) => f.dim + g.keyToInt(i)
    }

    override def intToKey(i: Int): Either[f.Key, g.Key] =
      if (i < f.dim) Left(f.intToKey(i)) else Right(g.intToKey(i - f.dim))

    override def sum[A](v: (F[A], G[A]))(implicit a: Additive[A]): A =
      a.add(f.sum(v._1), g.sum(v._2))
  }

  implicit def product[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit override val f: Cartesian[F], override val g: Cartesian[G])
    extends Representable.Compose[F, G] with Cartesian[({type T[A] = F[G[A]]})#T] {
    override val dim: Int = f.dim * g.dim

    override def keyToInt(k: (f.Key, g.Key)): Int =
      f.keyToInt(k._1) * g.dim + g.keyToInt(k._2)

    override def intToKey(i: Int): (f.Key, g.Key) =
      (f.intToKey(i / g.dim), g.intToKey(i % g.dim))

    override def sum[A](v: F[G[A]])(implicit a: Additive[A]): A =
      f.sum(f.map(v)(g.sum(_)))
  }

  implicit def compose[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new Compose[F, G]

  implicit object Zero extends Cartesian[({type T[A] = Unit})#T] {
    type Key = Void

    override val dim: Int = 0

    override def keyToInt(k: Void): Int =
      throw new IllegalArgumentException

    override def intToKey(i: Int): Void =
      throw new IllegalArgumentException

    override def zero[A](implicit a: Zero[A]): Unit = ()

    override def tabulate[A](v: (Void) => A)(implicit a: ClassTag[A]): Unit = ()

    override def sum[A](v: Unit)(implicit a: Additive[A]): A = a.zero

    override def index[A](v: Unit)(k: Void)(implicit a: ClassTag[A]): A =
      throw new NoSuchElementException

    override def apply2[A, B, C](x: Unit, y: Unit)(h: (A, B) => C)
        (implicit a: ClassTag[A], b: ClassTag[B], c: ClassTag[C]): Unit = ()

    override def map[A, B](v: Unit)(h: (A) => B)(implicit a: ClassTag[A], b: ClassTag[B]): Unit = ()

    override def restrict(keys: => Set[Void]) = new WholeSpace[({type T[A] = Unit})#T]()(this)

    override implicit def classTag[A](implicit a: ClassTag[A]): ClassTag[Unit] =
      new ClassTag[Unit] {
        override def runtimeClass: Class[_] = classOf[Unit]
      }
  }

  implicit object Scalar extends Cartesian[({type T[A] = A})#T] {
    type Key = Unit

    override val dim: Int = 1

    override def keyToInt(k: Unit): Int = 0

    override def intToKey(i: Int): Unit = ()

    override def zero[A](implicit a: Zero[A]): A = a.zero

    override def map[A, B](v: A)(h: (A) => B)(implicit a: ClassTag[A], b: ClassTag[B]): B = h(v)

    override def apply2[A, B, C](x: A, y: B)(h: (A, B) => C)
        (implicit a: ClassTag[A], b: ClassTag[B], c: ClassTag[C]): C = h(x, y)

    override def zip[A, B](x: A, y: B)
        (implicit a: ClassTag[A], b: ClassTag[B]): (A, B) = (x, y)

    override def ap[A, B](x: A)(h: (A) => B)
        (implicit a: ClassTag[A], b: ClassTag[B]): B = h(x)

    override def point[A](x: A)(implicit a: ClassTag[A]): A = x

    override def tabulate[A](v: (Unit) => A)(implicit a: ClassTag[A]): A = v()

    override def index[A](v: A)(k: Unit)(implicit a: ClassTag[A]): A = v

    override def sum[A](v: A)(implicit a: Additive[A]): A = v

    override def restrict(keys: => Set[Unit]) = new WholeSpace[({type T[A] = A})#T]()(this)

    override def classTag[A](implicit a: ClassTag[A]): ClassTag[A] =
      new ClassTag[A] {
        override def runtimeClass: Class[_] = a.runtimeClass
      }
  }
}
