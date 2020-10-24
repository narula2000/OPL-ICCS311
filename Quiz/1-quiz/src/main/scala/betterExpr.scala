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
  override def toVal(implicit ctx: Map[String, Double]): Double =
    /*
     * Neg(y) = -1 * y
     * Neg(y) = flip(y) + 1
     * Neg(y) - 1 = flip(y)
     * -1 * y - 1 = flip(y) // for max Double
     *  -1 * (y + 1) = flip(y) // for min Double
     */
    // -1.0 * (e.toVal + 1.0) this might cause over flow max(+sign) + 1
    // (-1.0 * e.toVal) - 1.0 this might cause over flow -(min(-sign)) > max(+sign)
    if (scala.Double.MaxValue == e.toVal) (-1.0 * e.toVal) - 1.0
    else if (scala.Double.MinValue == e.toVal) -1.0 * (e.toVal + 1.0)
    else -1.0 * (e.toVal + 1.0)
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
// ex.toVal(Map("x" -> 2.0))
// implicit val ctx: Map[String, Double] = Map("x" -> 2.0)
// println(nEx.toVal)
// println(ex.uanry_~.toVal)
// }
