package codeletgen.skeleton.fft

// An implementation of two algorithms for computing DFT and IDFT (Inverse DFT).
//
// 1) Direct DFT, per definition of DFT. (https://en.wikipedia.org/wiki/Discrete_Fourier_transform)
// 2) Fast Fourier Transform, namely Cooley-Tukey (https://en.wikipedia.org/wiki/Fast_Fourier_transform)
//
// As in [FFTW], we select between DFT and IDFT with teh sign parameter.
// Note that IDFT is not scaled by `n`.  See the note at Eq 2 in [FFTW]
//
// The implementation below is fixed to concrete values of type Complex[Double].  That is, the
// implementation is not yet lifted to alternative semantics needed to generate the expression dag.

import cats.implicits.{catsStdShowForVector, toShow}
import cats.Show

import spire.implicits._
import spire.math.prime.{Factors, isPrime}
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

import showForComplex._

object FFT {
  type T = Complex[Double]
  // arrays are modeled as functions from indices to values
  type Arr = Int => T

  // This function is a simplified version of the fftgen function from page 5 of [FFTW].
  // This version selects only between Cooley-Tukey and the direct algorithm.
  def fft(n: Int)(X: Arr)(sign: Int): Arr = {
    if (isPrime(n))
      direct(n)(X)(sign)
    else {
      // Factor n into n1, n2, for a call to Cooley-Tukey.
      // No attention is paid to obtaining performance-optimal n1, n2.
      val factors = Factors(n)
      val (n1, n2) = factors.toList match {
        case (b,e) :: List() => (b pow (e-1), b)   // n factors into b^e
        case (b,e) :: _      => (b pow e, n.toSafeLong / (b pow e))  // n factors to b^e * <more terms>
      }
      println(s"$n=$n1*$n2")
      assert(n1 * n2 == n)
      cooleyTukey(n1.toInt)(n2.toInt)(X)(sign)
    }
  }

  private def exp(n: Int, k: Int): T = Complex.rootOfUnity(n,k)

  // transcribed from Fig 4 in [FFTW]
  def cooleyTukey(n1: Int)(n2: Int)(X: Arr)(sign: Int): Arr = {
    def tmp1(j2: Int)          = fft(n1)(j1 => X(j1 * n2 + j2))(sign)
    def tmp2(i1: Int)(j2: Int) = exp(n1*n2, sign * i1 * j2) * tmp1(j2)(i1)
    def tmp3(i1: Int)          = fft(n2)(tmp2(i1))(sign)
    i => tmp3( i%n1 )( i/n1 )
  }

  def direct(n: Int)(X: Arr)(sign: Int): Arr = {
    i =>
      (
        for (j <- 0 until n) yield {
          val cij = exp(n, sign * i * j)
          // FYI: The following definition is from wikipedia (DFT). Not semantically identical to above but a legal DFT.
          // val cij = Complex[Double] (scala.math.cos(2*Pi*i*j/n), -scala.math.sin(2*Pi*i*j/n))
          X(j) * cij
        })
        .foldLeft(Complex[Double](0))(_ + _) // sum the list of terms
  }

}

object FFT_Test extends App {
  import FFT._

  // X4 is the example from wikipedia on DFT
  val X4 = Vector(
    Complex(1.0), Complex(2.0, -1.0),
    Complex(0.0, -1.0), Complex(-1.0, 2.0)
  )
  val X6 = Vector(
    Complex(1.0), Complex(2.0, -1.0), Complex(0.0, -1.0),
    Complex(1.0,0), Complex(1.0,2.0), Complex(0.0,-3.0)
  )
  val X10 = Vector(
    Complex(1.0,0), Complex(1.0,0),
    Complex(1.0,0), Complex(1.0,0),
    Complex(0.0,0), Complex(0.0,2),
    Complex(0.0,0), Complex(0.0,0),
    Complex(0.0,0), Complex(0.0,2)
  )

  val X = X10  // choose your input vector here

  val Y       = fft   (X.length)(X(_))(1)  // compute with CooleyTukey, when applicable
  val Ydirect = direct(X.length)(X(_))(1)

  val Z       = fft   (X.length)(Y(_))(-1)
  val Zdirect = direct(X.length)(Ydirect(_))(-1)

  println(X.show)
  println
  println(Vector.tabulate(X.length)(Y).show)
  println(Vector.tabulate(X.length)(Ydirect).show)
  println
  println(Vector.tabulate(X.length)(Z).show)
  println(Vector.tabulate(X.length)(Zdirect).show)
}