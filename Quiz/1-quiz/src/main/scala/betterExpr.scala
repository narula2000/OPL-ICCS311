trait Expr {
  def +(that: Expr) = Sum(this, that)
  def *(that: Expr) = Prod(this, that)
  def -(that: Expr) = Sub(this, that)
  def /(that: Expr) = Div(this, that)
  def uanry_- = Negate(this)
  def uanry_~ = LogNeg(this)
  def toVal(implicit ctx: Map[String, Double]): Double
}

case class Var(name: String) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    ctx.getOrElse(name, 0)
}

case class Constant(n: Double) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = n.toDouble
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    e1.toVal + e2.toVal
}

case class Sub(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    e1.toVal - e2.toVal
}

case class Prod(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    e1.toVal * e2.toVal
}

case class Negate(e: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = -1.0 * e.toVal
}

case class LogNeg(e: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = {
    /*
     * Neg(y) = -1 * y
     * Neg(y) = flip(y) + 1
     * Neg(y) - 1 = flip(y)
     * -1 * y - 1 = flip(y) // for max Double
     *  -1 * (y + 1) = flip(y) // for min Double
     */
    // -1.0 * (e.toVal + 1.0) this might cause over flow max(+sign) + 1
    // (-1.0 * e.toVal) - 1.0 this might cause over flow -(min(-sign)) > max(+sign)
    // if (scala.Double.MaxValue == e.toVal) (-1.0 * e.toVal) - 1.0
    // else if (scala.Double.MinValue == e.toVal) -1.0 * (e.toVal + 1.0)
    // else -1.0 * (e.toVal + 1.0)
    // I have tried all these then reliased that all Double can be decimal
    // and I also know it use Double 64 bit IEEE 754 double-precision float
    // Hence i decided to change value to binary and then flip it and
    // return the double value
    // Function to flip bit
    def flip(i: Char) = {
      if (i == '1') '0'
      else if (i == '0') '1'
      else '3'
    }
    // Create list of binary for flipped bits
    val buf = new StringBuilder
    // Flipping the bits with java help
    var binary =
      java.lang.Long.toBinaryString(
        java.lang.Double.doubleToRawLongBits(e.toVal)
      )
    if (e.toVal > 0) binary = '0' + binary // This was added for sign bit
    println(binary)
    binary.foreach(i => buf += flip(i.toChar)) // Flipping all bits
    println(buf.toString())
    // Combine all bits and Change back to Double with Java help
    val answer = java.lang.Double.longBitsToDouble(
      new java.math.BigInteger(buf.toString(), 2).longValue()
    )
    answer
  }
}

case class Div(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    try {
      if (e2.toVal == 0.0) { throw new ArithmeticException("Dviding by 0") }
      e1.toVal / e2.toVal
    } catch {
      case ex: ArithmeticException =>
        println(ex.getMessage())
        0.0
    }
}

// object Main extends App {
// val x = Var("x")
// val ex = ((x + Constant(5)) * x + Constant(11) * x)
// val nEx = ex.uanry_-
// val dp = Constant(3)
// ex.toVal(Map("x" -> 3.0))
// implicit val ctx: Map[String, Double] = Map("x" -> 3.0)
// println(nEx.toVal)
// println(dp.uanry_~.toVal)
// }
