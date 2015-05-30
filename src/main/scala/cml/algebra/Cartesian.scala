package cml.algebra

import shapeless.Nat
import shapeless.ops.nat.ToInt

/**
 * A functor that maps field F to the cartesian space F&#94;n for some natural number n.
 */
trait Cartesian[F[_]] extends Normed[F] with ZeroApplicative[F] {
  /**
   * Dimension of this vector space.
   */
  val dim: Int

  /**
   * Creates a vector with coordinates given by a function.
   */
  def tabulate[A](v: (Key) => A)(implicit a: Zero[A]): F[A]

  /**
   * Creates a new vector from a map. Coefficients for keys not in the map are zero.
   */
  final override def tabulate[A](v: Map[Key, A])(implicit a: Zero[A]): F[A] =
    tabulate((k: Key) => v.getOrElse(k, a.zero))

  /**
   * Returns the whole space.
   */
  final override def restrict(keys: Set[Key]): Subspace[F] =
    new Subspace.WholeSpace[F]()(this)

  /**
   * Lifts a value.
   */
  override def point[A](x: A)(implicit a: Zero[A]): F[A] = tabulate(_ => x)
}

object Cartesian {
  import ZeroFunctor.asZero

  class Product[F[_], G[_]] (implicit override val f: Cartesian[F], override val g: Cartesian[G])
    extends Normed.Product[F, G] with Cartesian[({type T[A] = (F[A], G[A])})#T] {
    override val dim: Int = f.dim + g.dim

    override def tabulate[A](v: (Either[f.Key, g.Key]) => A)(implicit a: Zero[A]): (F[A], G[A]) =
      (f.tabulate(k => v(Left(k))), g.tabulate(k => v(Right(k))))

    override def point[A](x: A)(implicit a: Zero[A]): (F[A], G[A]) =
      (f.point(x), g.point(x))
  }

  implicit def product[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit override val f: Cartesian[F], override val g: Cartesian[G])
    extends Normed.Compose[F, G] with Cartesian[({type T[A] = F[G[A]]})#T] {
    override val dim: Int = f.dim * g.dim

    override def tabulate[A](v: ((f.Key, g.Key)) => A)(implicit a: Zero[A]): F[G[A]] =
      f.tabulate(i => g.tabulate(j => v((i, j))))

    override def point[A](x: A)(implicit a: Zero[A]): F[G[A]] =
      f.point(g.point(x))
  }

  implicit def compose[F[_], G[_]](implicit f: Cartesian[F], g: Cartesian[G]) = new Compose[F, G]

  implicit object Zero extends Cartesian[({type T[A] = Unit})#T] {
    type Key = Void

    override val dim: Int = 0

    override def zero[A](implicit a: Zero[A]): Unit = ()

    override def tabulate[A](v: (Void) => A)(implicit a: Zero[A]): Unit = ()

    override def sum[A](v: Unit)(implicit a: Additive[A]): A = a.zero

    override def index[A](v: Unit)(k: Void)(implicit a: Zero[A]): A =
      throw new NoSuchElementException

    override def applyC2[A, B, C](x: Unit, y: Unit)(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Unit = ()

    override def apply2[A, B, C](x: Unit, y: Unit)(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Unit = ()

    override def map[A, B](v: Unit)(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): Unit = ()
  }

  implicit object Scalar extends Cartesian[({type T[A] = A})#T] {
    type Key = Unit

    override val dim: Int = 1

    override def zero[A](implicit a: Zero[A]): A = a.zero

    override def map[A, B](v: A)(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): B = h(v)

    override def apply2[A, B, C](x: A, y: B)(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): C = h(x, y)

    override def applyC2[A, B, C](x: A, y: B)(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): C = h(x, y)

    override def zip[A, B](x: A, y: B)
        (implicit a: Zero[A], b: Zero[B]): (A, B) = (x, y)

    override def ap[A, B](x: A)(h: (A) => B)
        (implicit a: Zero[A], b: Zero[B]): B = h(x)

    override def apC[A, B](x: A)(h: (A) => B)
        (implicit a: Zero[A], b: Zero[B]): B = h(x)

    override def point[A](x: A)(implicit a: Zero[A]): A = x

    override def tabulate[A](v: (Unit) => A)(implicit a: Zero[A]): A = v()

    override def index[A](v: A)(k: Unit)(implicit a: Zero[A]): A = v

    override def sum[A](v: A)(implicit a: Additive[A]): A = v
  }

  class VecImpl[S <: Nat](implicit size: ToInt[S])
    extends Cartesian[({type T[a] = Vec[S, a]})#T] {
    type Key = Int

    override val dim: Int = size()

    override def zero[A](implicit a: Zero[A]): Vec[S, A] =
      Vec(Array.fill(dim)(a.zero))

    override def map[A, B](v: Vec[S, A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): Vec[S, B] =
      Vec(Array.tabulate(dim)(i => h(v.get(i))))

    override def apply2[A, B, C](x: Vec[S, A], y: Vec[S, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Vec[S, C] =
      Vec(Array.tabulate(dim)(i => h(x.get(i), y.get(i))))

    override def applyC2[A, B, C](x: Vec[S, A], y: Vec[S, B])(h: (A, B) => C)
        (implicit a: Zero[A], b: Zero[B], c: Zero[C]): Vec[S, C] =
      Vec(Array.tabulate(dim)(i => h(x.get(i), y.get(i))))

    override def zip[A, B](x: Vec[S, A], y: Vec[S, B])
        (implicit a: Zero[A], b: Zero[B]): Vec[S, (A, B)] =
      Vec(Array.tabulate(dim)(i => (x.get(i), y.get(i))))

    override def ap[A, B](x: Vec[S, A])(h: Vec[S, (A) => B])
        (implicit a: Zero[A], b: Zero[B]): Vec[S, B] =
      Vec(Array.tabulate(dim)(i => h.get(i)(x.get(i))))

    override def apC[A, B](x: Vec[S, A])(h: Vec[S, (A) => B])
        (implicit a: Zero[A], b: Zero[B]): Vec[S, B] =
      Vec(Array.tabulate(dim)(i => h.get(i)(x.get(i))))

    override def point[A](x: A)(implicit a: Zero[A]): Vec[S, A] =
      Vec(Array.fill(dim)(x))

    override def tabulate[A](v: (Int) => A)(implicit a: Zero[A]): Vec[S, A] =
      Vec(Array.tabulate(dim)(v))

    override def index[A](v: Vec[S, A])(k: Int)(implicit a: Zero[A]): A =
      v.get(k)

    override def sum[A](v: Vec[S, A])(implicit a: Additive[A]): A =
      v.get.fold(a.zero)(a.add)
  }

  implicit def vec[S <: Nat](implicit toInt: ToInt[S]) = new VecImpl[S]
}
