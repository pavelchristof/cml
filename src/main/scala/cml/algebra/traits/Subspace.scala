package cml.algebra.traits

/**
 * Created by pawel on 21.05.15.
 */
trait Subspace[V[_]] {
  type Type[A]
  def inject[A](u: Type[A])(implicit a: Additive[A]): V[A]
  def project[A](v: V[A])(implicit a: Additive[A]): Type[A]
  implicit val concrete: Concrete[Type]
}

object Subspace {
  def refl[V[_]](implicit c: Concrete[V]) = new Subspace[V] {
    override type Type[A] = V[A]

    override def inject[A](u: V[A])(implicit a: Additive[A]): V[A] = u
    override def project[A](v: V[A])(implicit a: Additive[A]): V[A] = v

    override implicit val concrete: Concrete[Type] = c
  }
}
