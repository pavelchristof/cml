package cml.algebra

case class Reflector[Key] (
  keys: Set[Key]
)

object Reflector {
  class AnalyticInst[Key] extends Analytic[Reflector[Key]] {
    override val zero: Reflector[Key] = 
      Reflector(Set())
    
    override def add(x: Reflector[Key], y: Reflector[Key]): Reflector[Key] =
      Reflector(x.keys ++ y.keys)
    override def sub(x: Reflector[Key], y: Reflector[Key]): Reflector[Key] =
      Reflector(x.keys ++ y.keys)
    override def neg(x: Reflector[Key]): Reflector[Key] = x

    override val one: Reflector[Key] = 
      Reflector(Set())
    override def mul(x: Reflector[Key], y: Reflector[Key]): Reflector[Key] =
      Reflector(x.keys ++ y.keys)
      
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

    override def fromFloat(x: Float): Reflector[Key] = Reflector(Set())
    override def fromDouble(x: Double): Reflector[Key] = Reflector(Set())
  }

  implicit def analytic[Key] = new AnalyticInst[Key]
}
