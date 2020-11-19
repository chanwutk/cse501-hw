package codeletgen.skeleton.fft

import spire.algebra.Rng
import cats.Show
import spire.math.Complex

object showForAstNode {
  implicit val complexShow: Show[Complex[AstNode]] = Show.show(c => s"Complex(${c.real.toString}, ${c.imag.toString})")
}

sealed trait AstNode
case class AddNode(a: AstNode, b: AstNode) extends AstNode
case class MultNode(a: AstNode, b: AstNode) extends AstNode
case class NegNode(a: AstNode) extends AstNode
case class ConstNode(c: Double) extends AstNode

object AstNodeInstance {
  implicit def AstNodeTransformer: Transformer[AstNode] = (c: Complex[Double]) => Complex(ConstNode(c.real), ConstNode(c.imag))

  implicit def AstNodeRng: Rng[AstNode] = new Rng[AstNode] {
    override def negate(x: AstNode): AstNode = NegNode(x)
    override def zero: AstNode = ConstNode(0.0)
    override def times(x: AstNode, y: AstNode): AstNode = MultNode(x, y)
    override def plus(x: AstNode, y: AstNode): AstNode = AddNode(x, y)
  }
}

object AstNodeFFT extends App {
  import AstNodeInstance._
  import showForAstNode._
  import codeletgen.skeleton.fft.FFT_Test._

  _print(X10.map(c => AstNodeTransformer.transforms(c)))  // choose your input vector here
}
