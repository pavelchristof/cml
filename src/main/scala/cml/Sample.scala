package cml

import scalaz._

case class Sample[In, Out] (
  input: In,
  expected: Out,
  actual: Out
)

object Sample extends Bitraverse[Sample] {
  override def bitraverseImpl[G[_], A, B, C, D](fab: Sample[A, B])(f: (A) => G[C], g: (B) => G[D])
      (implicit app: Applicative[G]): G[Sample[C, D]] =
    app.apply3(f(fab.input), g(fab.expected), g(fab.actual))(Sample(_, _, _))
}
