import cml.ad
import cml.algebra.Analytic
import cml.algebra._
import org.scalacheck.Properties
import org.scalacheck.Prop._
import Floating._

object DifferentiationTest extends Properties("Differentiation") {
  trait Fun {
    def description: String
    def value[A](x: A)(implicit an: Analytic[A]): A
    def deriv[A](x: A)(implicit an: Analytic[A]): A
  }

  val engines = Seq(("Forward", ad.Forward), ("Backward", ad.Backward))

  val funs = Seq(
    new Fun {
      override def description = "constant"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        an.fromDouble(42).abs + an.fromInt(32) * an.fromDouble(12).signum
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _0
      }
    },
    new Fun {
      override def description = "polynomial"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x + x * x - fromInt(3) * x + fromInt(2) * x * x * x * x
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _1 + fromInt(2) * x - fromInt(3) + fromInt(8) * x * x * x
      }
    },
    new Fun {
      override def description = "polynomial2"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (x * x * x * x).sqrt
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (fromInt(2) * x * x * x) / (x * x * x * x).sqrt
      }
    },
    new Fun {
      override def description = "exp"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.exp
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.exp
      }
    },
    new Fun {
      override def description = "exp after log"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.log.exp
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _1
      }
    },
    new Fun {
      override def description = "double sine"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (x * fromInt(2)).sin - x.sin * x.cos * fromInt(2)
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _0
      }
    },
    new Fun {
      override def description = "double cosine"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (x * fromInt(2)).cos - x.cos.square + x.sin.square
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _0
      }
    },
    new Fun {
      override def description = "double tangent"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (fromInt(2) * x).tan - (fromInt(2) * x.tan) / (_1 - x.tan.square)
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _0
      }
    },
    new Fun {
      override def description = "cos^2 x + sin^2 x = 1"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.cos.square + x.sin.square
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        -_2 * x.sin * x.cos + (_2 * x).sin
      }
    },
    new Fun {
      override def description = "sin"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.sin
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.cos
      }
    },
    new Fun {
      override def description = "cos"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.cos
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        -x.sin
      }
    },
    new Fun {
      override def description = "sin after asin"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.asin.sin
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _1
      }
    },
    new Fun {
      override def description = "cos after acos"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.acos.cos
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _1
      }
    },
    new Fun {
      override def description = "tan after atan"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.atan.tan
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _1
      }
    },
    new Fun {
      override def description = "cosh^2 - sinh^2 = 1"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.cosh.square - x.sinh.square
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _0
      }
    },
    new Fun {
      override def description = "tanh definition"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x.tanh - (x.exp - (-x).exp) / (x.exp + (-x).exp)
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        _0
      }
    }
  )

  def closeEnough(x: Double, y: Double): Boolean = {
    val eps = 1e-2
    x == y || (x - y).abs <= eps * (x.abs.max(y.abs).max(eps))
  }

  for ((name, engine) <- engines) {
    import engine._
    for (fun <- funs) {
      def prepFun(x: Aug[Double], ctx: Context[Double]): Aug[Double] =
        fun.value(x)(analytic(Floating.DoubleInst, ctx))
      val computedDeriv = diffWithValue[Double](prepFun)
      property(s"$name.${fun.description}") = forAll { (x: Double) => {
        val eVal = fun.value(x)
        val eDiff = fun.deriv(x)
        val (aVal, aDiff) = computedDeriv(x)
        val actual = aDiff
        val expected = eDiff
        x.abs > 1e7 || x.abs < 1e-7 || expected.isNaN || actual.isNaN || closeEnough(expected, actual) || {
          println(s"Expected: $expected, actual $actual")
          false
        }
      }}
    }
  }
}
