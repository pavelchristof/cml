package cml.algebra

import scala.collection.immutable.HashSet

class Reflector[Key] (
  val cell: Reflector.Cell[Key]
) {
  def extract(): Set[Key] = Reflector.extract(cell)
}

object Reflector {
  sealed abstract class Cell[Key] {
    var visited = false
  }
  case class Empty[Key] () extends Cell[Key]
  case class Input[Key] (key: Key) extends Cell[Key]
  case class Node[Key] (x: Cell[Key], y: Cell[Key]) extends Cell[Key]

  def apply[Key](key: Key): Reflector[Key] =
    new Reflector[Key](Input(key))

  def extract[Key](root: Cell[Key]): Set[Key] = {
    val b = HashSet.newBuilder[Key]
    def traverse(cell: Cell[Key]): Unit =
      if (!cell.visited) {
        cell.visited = true
        cell match {
          case Empty() => ()
          case Input(key) => b += key
          case Node(x, y) => {
            traverse(x)
            traverse(y)
          }
        }
      }
    traverse(root)
    b.result()
  }

  class AnalyticInst[Key] extends Analytic[Reflector[Key]] {
    override val zero: Reflector[Key] = 
      new Reflector(Empty())
    
    override def add(x: Reflector[Key], y: Reflector[Key]): Reflector[Key] =
      new Reflector(Node(x.cell, y.cell))
    override def sub(x: Reflector[Key], y: Reflector[Key]): Reflector[Key] =
      new Reflector(Node(x.cell, y.cell))
    override def neg(x: Reflector[Key]): Reflector[Key] = x

    override val one: Reflector[Key] =
      new Reflector(Empty())
    override def mul(x: Reflector[Key], y: Reflector[Key]): Reflector[Key] =
      new Reflector(Node(x.cell, y.cell))
      
    override def inv(x: Reflector[Key]): Reflector[Key] = x
    
    override def atan(x: Reflector[Key]): Reflector[Key] = x
    override def acos(x: Reflector[Key]): Reflector[Key] = x
    override def tanh(x: Reflector[Key]): Reflector[Key] = x
    override def log(x: Reflector[Key]): Reflector[Key] = x
    override def cosh(x: Reflector[Key]): Reflector[Key] = x
    override def tan(x: Reflector[Key]): Reflector[Key] = x
    override def cos(x: Reflector[Key]): Reflector[Key] = x
    override def exp(x: Reflector[Key]): Reflector[Key] = x
    override def asin(x: Reflector[Key]): Reflector[Key] = x
    override def abs(x: Reflector[Key]): Reflector[Key] = x
    override def sqrt(x: Reflector[Key]): Reflector[Key] = x
    override def sin(x: Reflector[Key]): Reflector[Key] = x
    override def signum(x: Reflector[Key]): Reflector[Key] = x
    override def sinh(x: Reflector[Key]): Reflector[Key] = x

    override def fromFloat(x: Float): Reflector[Key] =
      new Reflector(Empty())
    override def fromDouble(x: Double): Reflector[Key] =
      new Reflector(Empty())
  }

  implicit def analytic[Key] = new AnalyticInst[Key]
}
