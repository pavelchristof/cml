package cml

/**
 * Recursively enumerable types.
 */
trait Enumerate[T] {
  /**
   * The number of elements, if the type is finite.
   */
  val count: Option[BigInt]

  /**
   * Enumerates all the values of this type.
   */
  def enumerate: Stream[T]
}

object Enumerate {
  /**
   * Enumerates all non-negative Ints up to n.
   */
  def natInt(n: Int): Enumerate[Int] = new Enumerate[Int] {
    override val count: Option[BigInt] = Some(n.max(0))
    override def enumerate: Stream[Int] = (0 until n).toStream
  }

  /**
   * Enumerates all natural numbers up to n.
   */
  def natBigInt(n: BigInt): Enumerate[BigInt] = new Enumerate[BigInt] {
    override val count: Option[BigInt] = Some(n.max(0))
    override def enumerate: Stream[BigInt] = (BigInt(0).until(n)).toStream
  }

  /**
   * Enumerate all chars.
   */
  def char: Enumerate[Char] = new Enumerate[Char] {
    override val count: Option[BigInt] = Some(BigInt(Char.MaxValue) - BigInt(Char.MinValue))
    override val enumerate: Stream[Char] = (Char.MinValue until Char.MaxValue).toStream
  }

  def charDigit: Enumerate[Char] = new Enumerate[Char] {
    override val count: Option[BigInt] = Some(10)
    override def enumerate: Stream[Char] = Stream('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  }

  /**
   * Enumerates all strings over some alphabete.
   */
  def string(charEnum: Enumerate[Char]): Enumerate[String] = new Enumerate[String] {
    override val count: Option[BigInt] = None
    override def enumerate: Stream[String] =
      Stream.cons("", enumerate.flatMap(s => charEnum.enumerate.map(s + _)))
  }

  /**
   * Enumerates all values of type Either[A, B] for enumerable A and B.
   */
  def sum[A, B](implicit a: Enumerate[A], b: Enumerate[B]): Enumerate[Either[A, B]] = new Enumerate[Either[A, B]] {
    override val count: Option[BigInt] = a.count.flatMap(x => b.count.map(x + _))
    override def enumerate: Stream[Either[A, B]] = goLeft(a.enumerate, b.enumerate)

    private def goLeft(l: Stream[A], r: Stream[B]): Stream[Either[A, B]] =
      l match {
        case Stream.cons(a, as) => Stream.cons(Left(a), goRight(as, r))
        case Stream.Empty => r.map(Right(_))
      }

    private def goRight(l: Stream[A], r: Stream[B]): Stream[Either[A, B]] =
      r match {
        case Stream.cons(b, bs) => Stream.cons(Right(b), goLeft(l, bs))
        case Stream.Empty => l.map(Left(_))
      }
  }

  /**
   * Enumerates all pairs (A, B) for enumerable A and B.
   */
  def product[A, B](implicit a: Enumerate[A], b: Enumerate[B]): Enumerate[(A, B)] = new Enumerate[(A, B)] {
    override val count: Option[BigInt] = a.count.flatMap(x => b.count.map(x * _))
    override def enumerate: Stream[(A, B)] = goLeft(Stream.empty, Stream.empty, a.enumerate, b.enumerate)

    private def goLeft(l: Stream[A], r: Stream[B], lt: Stream[A], rt: Stream[B]): Stream[(A, B)] =
      lt match {
        case Stream.cons(a, as) => r.map((a, _)) ++ goRight(Stream.cons(a, l), r, as, rt)
        case Stream.Empty => rt.flatMap(b => l.map((_, b)))
      }
    private def goRight(l: Stream[A], r: Stream[B], lt: Stream[A], rt: Stream[B]): Stream[(A, B)] =
      rt match {
        case Stream.cons(b, bs) => l.map((_, b)) ++ goLeft(l, Stream.cons(b, r), lt, bs)
        case Stream.Empty => lt.flatMap(a => r.map((a, _)))
      }
  }
}
