import cml.Enumerate

object EnumerateTest extends App {
  val strings = Enumerate.string(Enumerate.charDigit)
  strings.enumerate.take(100).print()
  println()
  val pairs = Enumerate.product(Enumerate.natInt(3), strings)
  pairs.enumerate.take(100).print()
}
