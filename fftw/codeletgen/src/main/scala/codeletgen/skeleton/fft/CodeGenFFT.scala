package codeletgen.skeleton.fft

import cats.implicits.toShow
import spire.algebra.Rng
import cats.Show
import codeletgen.skeleton.fft.FFT.{direct, fft}
import spire.math.Complex

object showForCodeGen {
  import CodeGenInstance._

  implicit val complexShow: Show[Complex[CodeGen]] = Show.show(c => s"${c.real} + (${c.imag})i")

  implicit val vectorComplexShow: Show[Vector[Complex[CodeGen]]] = Show.show(v => {
    var ret = "struct complex { double re, im; };\n\n"
    ret += s"void generatedCode(struct complex in[${v.size}], struct complex out[${v.size}]) {\n"
    ret += _out()
    v.zipWithIndex.foreach{case (c, i) =>
      ret += s"  out[$i].re = ${_show(c.real)};\n"
      ret += s"  out[$i].im = ${_show(c.imag)};\n"
    }
    ret += "}\n\n\n"
    _reset()
    ret
  })

}

sealed trait CodeGen
case class ConstGen(c: String) extends CodeGen
case class VarGen(i: Int) extends CodeGen

object CodeGenInstance {
  private var idx = 0
  private var out = ""

  def _reset(): Unit = {
    idx = 0
    out = ""
  }

  def _out(): String = out

  def _show(x: CodeGen): String = {
    x match {
      case c: ConstGen =>
        if (c.c.startsWith("-")) {
          s"(${c.c})"
        } else {
          c.c
        }
      case v: VarGen => s"temp${v.i}"
    }
  }

  implicit def CodeGenTransformer: Transformer[CodeGen] = (c: Complex[Double]) => Complex[CodeGen](
    ConstGen(c.real.toString),
    ConstGen(c.imag.toString),
  )

  implicit def CodeGenRng: Rng[CodeGen] = new Rng[CodeGen] {
    private def _negate(x: String): String = {
      if (x.startsWith("-")) {
        x.substring(1)
      } else {
        "-" + x
      }
    }

    private def _append(exp: String): VarGen = {
      idx += 1
      out += s"  double temp$idx = $exp;\n"
      VarGen(idx)
    }

    override def negate(x: CodeGen): CodeGen = {
      x match {
        case c: ConstGen => ConstGen(_negate(c.c))
        case _: VarGen => _append(s"-${_show(x)}")
      }
    }

    override def zero: CodeGen = ConstGen("0.0")
    override def times(x: CodeGen, y: CodeGen): CodeGen = _append(s"${_show(x)} * ${_show(y)}")
    override def plus(x: CodeGen, y: CodeGen): CodeGen = _append(s"${_show(x)} + ${_show(y)}")
  }
}

object CodeGenFFT extends App {
  import CodeGenInstance._
  import showForCodeGen._
  import codeletgen.skeleton.fft.FFT_Test._

  var X = X10.zipWithIndex.map{case (_, i) => Complex[CodeGen](ConstGen(s"in[$i].re"), ConstGen(s"in[$i].im"))}  // choose your input vector here

  val Y       = fft   (X.length)(X(_))(1)  // compute with CooleyTukey, when applicable
  val Ydirect = direct(X.length)(X(_))(1)

  val Z       = fft   (X.length)(Y(_))(-1)
  val Zdirect = direct(X.length)(Ydirect(_))(-1)

  println(X.map(c => c.show).reduce(_ + ",\n" + _))
  println
  println(Vector.tabulate(X.length)(Y).show)
  println(Vector.tabulate(X.length)(Ydirect).show)
  println
  println(Vector.tabulate(X.length)(Z).show)
  println(Vector.tabulate(X.length)(Zdirect).show)
}
