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
import spire.algebra.{Eq, Field, Trig}
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

  implicit val doubleShow: Show[Complex[(Double, String, Int)]] = Show.show(c => Complex[Double](c.real._1, c.imag._1).toString)
}

import showForComplex._

object GenInstance {
  private type T = (Double, String, Int)
  private var idx: Int = 0

  def reset(): Unit = {
    idx = 0
  }

  def toGen[T1](x: Complex[T1]): Complex[T] = {
    (x.real, x.imag) match {
      case (real: Double, imag: Double) => Complex((real, s"$real", -1), (imag, s"$imag", -1))
      case (real: String, imag: String) => Complex((Double.NaN, real, -1), (Double.NaN, imag, -1))
      case (_, _) => Complex((Double.NaN, "Unknown", -1), (Double.NaN, "Unknown", -1))
    }
  }

  implicit def GenField: Field[T] = new Field[T] {
    override def zero: T = (0.0, "0.0", -1)
    override def one: T = (1.0, "1.0", -1)
    override def gcd(a: T, b: T)(implicit ev: Eq[T]): T = binary(a, b, "gcd", spire.math.gcd[Double])
    override def lcm(a: T, b: T)(implicit ev: Eq[T]): T = binary(a, b, "lcm", spire.math.lcm[Double])
    override def div(x: T, y: T): T = binary_op(x, y, "/", (a, b) => a / b)
    override def plus(x: T, y: T): T = binary_op(x, y, "+", (a, b) => a + b)
    override def times(x: T, y: T): T = binary_op(x, y, "*", (a, b) => a * b)

    override def negate(x: T): T = {
      x match {
        case (v, s, -1) => (-v, s"-($s)", -1)
        case (v, s, _) =>
          new_temp()
          print("-")
          _print(x)
          println(";")
          (-v, s"-($s)", idx)
      }
    }
  }

  implicit def GenTrig: Trig[T] = new Trig[T] {
    override def e: T = (spire.math.e, s"${spire.math.e}", -1)
    override def pi: T = (spire.math.pi, s"${spire.math.pi}", -1)
    override def exp(a: T): T = unary(a, "exp", spire.math.exp)
    override def expm1(a: T): T = unary(a, "expm1", spire.math.expm1)
    override def log(a: T): T = unary(a, "log", spire.math.log)
    override def log1p(a: T): T = unary(a, "log1p", spire.math.log1p)
    override def sin(a: T): T = unary(a, "sin", spire.math.sin)
    override def cos(a: T): T = unary(a, "cos", spire.math.cos)
    override def tan(a: T): T = unary(a, "tan", spire.math.tan)
    override def sinh(a: T): T = unary(a, "sinh", spire.math.sinh)
    override def cosh(a: T): T = unary(a, "cosh", spire.math.cosh)
    override def tanh(a: T): T = unary(a, "tanh", spire.math.tanh)
    override def asin(a: T): T = unary(a, "asin", spire.math.asin)
    override def acos(a: T): T = unary(a, "acos", spire.math.acos)
    override def atan(a: T): T = unary(a, "atan", spire.math.atan)
    override def atan2(y: T, x: T): T = binary(y, x, "atan2", spire.math.atan2)

    override def toRadians(a: T): T = {
      new_temp()
      _print(a)
      println(s" * ${spire.math.pi} / 180.0;")
      (spire.math.toRadians(a._1), s"${a._2} * ${spire.math.pi} / 180.0", idx)
    }

    override def toDegrees(a: T): T = {
      new_temp()
      _print(a)
      println(s" * 180.0 / ${spire.math.pi};")
      (spire.math.toDegrees(a._1), s"${a._2} * 180.0 / ${spire.math.pi}", idx)
    }
  }

  private def _print(x: T): Unit = {
    x match {
      case (_, s, -1) => print(s)
      case (_, _, i) => print(s"temp$i")
    }
  }

  private def new_temp(): Unit = {
    idx += 1
    print(s"  double temp$idx = ")
  }

  private def unary(a: T, fn_str: String, fn: Double => Double): T = {
    new_temp()
    print(s"$fn_str(")
    _print(a)
    println(");")
    (fn(a._1), s"$fn_str(${a._2})", idx)
  }

  private def binary(a: T, b: T, fn_str: String, fn: (Double, Double) => Double): T = {
    new_temp()
    print(s"$fn_str(")
    _print(a)
    print(", ")
    _print(b)
    println(");")
    (fn(a._1, b._1), s"$fn_str(${a._2}, ${b._2})", idx)
  }

  private def binary_op(a: T, b: T, op_str: String, op: (Double, Double) => Double): T = {
    new_temp()
    _print(a)
    print(s" $op_str ")
    _print(b)
    println(";")
    (op(a._1, b._1), s"(${a._2}) $op_str (${b._2})", idx)
  }
}

object FFT {
  import GenInstance._

  type T = Complex[(Double, String, Int)]
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
        .foldLeft(toGen(Complex[Double](0)))(_ + _) // sum the list of terms
  }

}

object FFT_Test extends App {
  import FFT._
  import GenInstance._

  def gen(len: Int, Y: Arr): Vector[FFT.T] = {
    reset()
    println("struct complex { double re, im; };")
    println
    println(s"void generatedCode(struct complex in[$len], struct complex out[$len]) {")
    val res = Vector.tabulate(X.length)(Y)
    res.zipWithIndex.foreach{case (c, idx) =>
      println(s"  out[$idx].re = temp${c.real._3};")
      println(s"  out[$idx].im = temp${c.imag._3};")
    }
    println("}")
    res
  }

  val hr = "----------------------------------------------------------------------"
  def title(str: String): Unit = {
    println
    println
    println
    println
    println(hr)
    println((" " * ((hr.length / 2) - (str.length / 2))) + str)
    println(hr)
    println
  }

  def XStr(n: Int): Vector[Complex[String]] = (0 until n).toVector.map(i => Complex(s"in[$i].re", s"in[$i].im"))

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

  val X = XStr(4).map(c => toGen(c))  // choose your input vector here
//  val X = X4.map(c => toGen(c))  // choose your input vector here
//  val X = X6.map(c => toGen(c))  // choose your input vector here
//  val X = X10.map(c => toGen(c))  // choose your input vector here

  val Y       = fft   (X.length)(X(_))(1)  // compute with CooleyTukey, when applicable
  val Ydirect = direct(X.length)(X(_))(1)

  val Z       = fft   (X.length)(Y(_))(-1)
  val Zdirect = direct(X.length)(Ydirect(_))(-1)

  println(X.show)

  title("Y")
  println(gen(X.length, Y).show)

  title("Ydirect")
  println(gen(X.length, Ydirect).show)

  title("Z")
  println(gen(X.length, Z).show)

  title("Zdirect")
  println(gen(X.length, Zdirect).show)
}