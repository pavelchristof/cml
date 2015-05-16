package cml

import scalaz.{Bitraverse, Applicative}

case class ScoredSample[In, Out] (
  input: In,
  expected: Out,
  actual: Out
)

object ScoredSample extends Bitraverse[ScoredSample] {
  override def bitraverseImpl[G[_], A, B, C, D](fab: ScoredSample[A, B])(f: (A) => G[C], g: (B) => G[D])
      (implicit app: Applicative[G]): G[ScoredSample[C, D]] =
    app.apply3(f(fab.input), g(fab.expected), g(fab.actual))(ScoredSample(_, _, _))
}
