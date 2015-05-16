import cml.algebra.ad
import cml.algebra.traits._
import cml.algebra.Real._
import org.scalacheck.Properties
import org.scalacheck.Prop._

object DifferentiationTest extends Properties("Differentiation") {
  trait Fun {
    def description: String
    def value[A](x: A)(implicit an: Analytic[A]): A
    def deriv[A](x: A)(implicit an: Analytic[A]): A
  }

  val engines = Seq(("Forward", ad.Forward))

  val funs = Seq(
    new Fun {
      override def description = "constant"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        42
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        0
      }
    },
    new Fun {
      override def description = "polynomial"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        x + x * x - x * 3 + x * x * x * x * 2
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        an.one + x * 2 - 3 + x * x * x * 8
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
      override def description = "double sine"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (x * 2).sin - x.sin * x.cos * 2
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        0
      }
    },
    new Fun {
      override def description = "double cosine"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (x * 2).cos - x.cos.square + x.sin.square
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        0
      }
    },
    new Fun {
      override def description = "double tangent"
      override def value[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        (x * 2).tan - (x.tan * 2) / (an.one - x.tan.square)
      }
      override def deriv[A](x: A)(implicit an: Analytic[A]): A = {
        import an.analyticSyntax._
        0
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
        0
      }
    }
  )

  def closeEnough(x: Double, y: Double): Boolean = {
    val eps = 0.0001
    x == y || (x - y).abs <= eps * (x.abs.max(y.abs).max(eps))
  }

  for ((name, engine) <- engines) {
    import engine._
    for (fun <- funs) {
      val computedDeriv = diff[Double](fun.value(_))
      property(s"$name.${fun.description}") = forAll { (x: Double) => {
        val expected = fun.deriv(x)
        val actual = computedDeriv(x)
        expected.isNaN || actual.isNaN || closeEnough(expected, actual)
      }}
    }
  }
}
