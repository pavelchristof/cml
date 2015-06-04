package cml.algebra

/**
 * An functor from the category of pointed types to the category of types.
 */
trait ZeroFunctor[F[_]] extends Serializable {
  /**
   * Lifts a zero preserving function.
   */
  def map[A, B](v: F[A])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): F[B]
}

object ZeroFunctor {
  import ZeroEndofunctor.asZero

  class Product[F[_], G[_]] (implicit f: ZeroFunctor[F], g: ZeroFunctor[G])
    extends ZeroFunctor[({type T[A] = (F[A], G[A])})#T] {
    override def map[A, B](v: (F[A], G[A]))(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): (F[B], G[B]) =
      (f.map(v._1)(h), g.map(v._2)(h))
  }

  implicit def product[F[_], G[_]](implicit f: ZeroFunctor[F], g: ZeroFunctor[G]) = new Product[F, G]

  class Compose[F[_], G[_]] (implicit f: ZeroFunctor[F], g: ZeroEndofunctor[G])
    extends ZeroFunctor[({type T[A] = F[G[A]]})#T] {
    override def map[A, B](v: F[G[A]])(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): F[G[B]] =
      f.map(v)(g.map(_)(h))
  }

  implicit def compose[F[_], G[_]](implicit f: ZeroFunctor[F], g: ZeroEndofunctor[G]) = new Compose[F, G]

  class ConstImpl[C] extends ZeroFunctor[({type T[A] = C})#T] {
    override def map[A, B](v: C)(h: (A) => B)(implicit a: Zero[A], b: Zero[B]): C = v
  }

  def const[C] = new ConstImpl[C]
}
