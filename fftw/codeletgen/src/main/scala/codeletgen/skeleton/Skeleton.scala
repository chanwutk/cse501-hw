package codeletgen.skeleton
import spire.math.{Complex}

// The type class with the operations in our language.
// Add here operations that will need to exist in the IR.
sealed trait Operations[T] {
  def const(c: Complex[Double]): T
  def load(i: Int): T
  def add(a: T, b: T): T
  def mult(a: T, b: T): T
}

sealed trait AstNode {
  def exec[T](o: Operations[T]): T
}
final case class AddNode(a: AstNode, b: AstNode) extends AstNode {
  def exec[T](o: Operations[T]) = o.add(a.exec(o), b.exec(o))
}
final case class MultNode(a: AstNode, b: AstNode) extends AstNode {
  def exec[T](o: Operations[T]) = o.mult(a.exec(o), b.exec(o))
}
final case class ConstNode(c: Complex[Double]) extends AstNode {
  def exec[T](o: Operations[T]) = o.const(c)
}
final case class LoadNode(i: Int) extends AstNode {
  def exec[T](o: Operations[T]) = o.load(i)
}

object OperationsInstances {
  // The semantics for direct execution of our DSL programs.

  // Note the environment, which is used to provide the values for loads.
  def IntOps(env: Int => Int): Operations[Int] = new Operations[Int] {
    def add(a: Int, b: Int): Int = a + b
    def mult(a: Int, b: Int): Int = a * b
    def const(c: Complex[Double]): Int = c.real.toInt
    def load(i: Int): Int = env(i)
  }

  // We are renaming String to Print just to denote this type
  // denotes the Printing semantics
  type Print = String

  def PrintOps(env: Int => Print) = new Operations[Print] {
    def add(a: Print, b: Print): Print = s"($a + $b)"
    def const(c: Complex[Double]): Print = c.toString
    def mult(a: Print, b: Print): Print = s"($a * $b)"
    def load(i: Int): Print = env(i)
  }

  def ComplexOps(env: Int => Complex[Double]) = new Operations[Complex[Double]] {
    def add(a: Complex[Double], b: Complex[Double]): Complex[Double] = Complex[Double](a.real + b.real, a.imag + b.imag)
    def mult(a: Complex[Double], b: Complex[Double]): Complex[Double] = Complex[Double](a.real * b.real - (a.imag * b.imag), a.real * b.imag + b.real * a.imag)
    def const(c: Complex[Double]): Complex[Double] = c
    def load(i: Int): Complex[Double] = env(i)
  }

  def AstNodeOps() = new Operations[AstNode] {
    def add(a: AstNode, b: AstNode): AstNode = AddNode(a, b)
    def mult(a: AstNode, b: AstNode): AstNode = MultNode(a, b)
    def const(c: Complex[Double]): AstNode = ConstNode(c)
    def load(i: Int): AstNode = LoadNode(i)
  }
}

// The type class interface (Interface syntax, see Scala with Cats, Chapter 1)
object OperationsSyntax {

  implicit class SyntaxOperations[T](a: T) {
    def +(b: T)(implicit o: Operations[T]): T = o.add(a, b)
    def *(b: T)(implicit o: Operations[T]): T = o.mult(a, b)
  }

  implicit class SyntaxConstantOperations(c: Complex[Double]) {
    def const[T](implicit o: Operations[T]): T = o.const(c)
  }

  implicit class SyntaxLoadOperations(c: Int) {
    def load[T](implicit o: Operations[T]): T = o.load(c)
  }
}

class SimpleExamples[T](implicit o: Operations[T]) {
  import OperationsSyntax._

  // here are little programs in our DSL.  The semantics
  // is specified by the type parameter `T`.  Here, it can
  // be anything since `T` is not bound here.

  def expr1: T = Complex[Double](1, 2).const + Complex[Double](2, 3).const * Complex[Double](3, 4).const

  def expr2: T = expr2(Complex[Double](1, 2).const, Complex[Double](2, 3).const)

  private def expr2(x: T, y: T): T = x + y * x

  def pow2n : T = pow(Complex[Double](2, 3).const, 5)

  // The `pow` functions is staged (`n` is a compile-time value,
  // and `x` is a run-time value.
  private def pow(x : T, n : Int) : T = {
    if (n == 1)
      x
    else
      x * pow(x, n-1)
  }
  // an example to show how we are printing loads.
  def load : T = {
    val tmp1 = 1.load + 2.load
    val tmp2 = 3.load * Complex[Double](1, 2).const
    tmp1 + tmp2
  }
}

object Test extends App {
  import OperationsInstances._

  // instantiate the direct semantics and the printing semantics
  val intOps = IntOps( Map(1->1, 2->2, 3->3, 4->4) )
  val printOps = PrintOps( i=>s"input[$i]" )
  val exI = new SimpleExamples()(intOps)
  val exP = new SimpleExamples()(printOps)
  val exA = new SimpleExamples()(AstNodeOps())

  // evaluate all examples under both semantics

  println( exI.expr1 )
  println( exA.expr1.exec(intOps) )
  println( exP.expr1 )
  println( exA.expr1.exec(printOps) )
  println( exA.expr1 )
  println()

  println( exI.expr2 )
  println( exA.expr2.exec(intOps) )
  println( exP.expr2 )
  println( exA.expr2.exec(printOps) )
  println( exA.expr2 )
  println()

  println( exI.pow2n )
  println( exA.pow2n.exec(intOps) )
  println( exP.pow2n )
  println( exA.pow2n.exec(printOps) )
  println( exA.pow2n )
  println()

  println( exI.load )
  println( exA.load.exec(intOps) )
  println( exP.load )
  println( exA.load.exec(printOps) )
  println( exA.load )
  println()
}