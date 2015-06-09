package cml.models

import cml.ParameterlessModel
import cml.algebra.{Analytic, Monoid1}

case class MonoidAppend[V[_]] (
  implicit monoid1: Monoid1[V]
) extends ParameterlessModel[({type T[A] = (V[A], V[A])})#T, V] {
  def apply[A](in: (V[A], V[A]))(implicit a: Analytic[A]): V[A] = monoid1.append(in._1, in._2)
}
