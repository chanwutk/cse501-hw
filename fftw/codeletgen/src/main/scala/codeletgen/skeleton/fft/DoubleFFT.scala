package codeletgen.skeleton.fft

import cats.Show
import spire.implicits._
import spire.math.{Complex, abs}

// Define a custom print function for Complex.
// See Chapter 1 in "Scala with Cats" to learn about the SHow type class.
object showForComplex {
  implicit val complexShow: Show[Complex[Double]] = Show.show(c => {
    // the following is based on from https://rosettacode.org/wiki/Fast_Fourier_transform#Scala, (which was buggy)
    val a = "%1.3f" format c.real
    val b = "%1.3f" format abs(c.imag)
    (a, b) match {
      case (_, "0.000") => a
      case ("0.000", _) if c.imag > 0 => b + "i"
      case ("0.000", _) => "-" + b + "i"
      case ("-0.000", _) if c.imag > 0 => b + "i"
      case ("-0.000", _) => "-" + b + "i"
      case (_, _) if c.imag > 0 => a + " + " + b + "i"
      case (_, _) => a + " - " + b + "i"
    }
  })
}

object DoubleInstance {
  implicit def DoubleTransformer: Transformer[Double] = (c: Complex[Double]) => c
}

object DoubleFFT extends App {
  import DoubleInstance._
  import showForComplex._
  import codeletgen.skeleton.fft.FFT_Test._

  _print(X10)  // choose your input vector here
}
