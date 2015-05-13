package cml.algebra.traits

abstract class AnalyticMap {
   def apply[F](x: F)(implicit f: Analytic[F]): F
   def apply[V[_], F](v: V[F])(implicit f: Analytic[F], c: Concrete[V]): V[F] =
     c.map(v)(this.apply(_))
}

object AnalyticMap {
  val abs = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.abs(x)
  }

  val signum = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.signum(x)
  }

  val exp = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.exp(x)
  }
  
  val log = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.log(x)
  }
  
  val sqrt = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.sqrt(x)
  }

  val sin = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.sin(x)
  }
  
  val cos = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.cos(x)
  }
  
  val tan = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.tan(x)
  }

  val asin = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.asin(x)
  }
  
  val acos = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.acos(x)
  }

  val atan = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.atan(x)
  }

  val sinh = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.sinh(x)
  }

  val cosh = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.cosh(x)
  }

  val tanh = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.tanh(x)
  }

  val sigmoid = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = {
      import f.analyticSyntax._
      f.one / (f.one + (-x).exp)
    }
  }

  val softmax = new AnalyticMap {
    override def apply[F](x: F)(implicit f: Analytic[F]): F = f.one
    override def apply[V[_], F](v: V[F])(implicit f: Analytic[F], c: Concrete[V]): V[F] = {
      import f.analyticSyntax._
      val s = c.sum(v)
      c.map(v)(_ / s)
    }
  }
}
