trait Expr {
  def +(that: Expr) = Sum(this, that)
  def *(that: Expr) = Prod(this, that)
  def uanry_- = Negate(this)
  def ~(that: Expr) = Negate(this)
  def toVal(implicit ctx: Map[String, Double]): Double
}

case class Var(name: String) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    ctx.getOrElse(name, 0)
}

case class Constant(n: Double) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = n.toDouble
}

case class Negate(e: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double = -1.0 * e.toVal
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    e1.toVal + e2.toVal
}

case class Prod(e1: Expr, e2: Expr) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    e1.toVal * e2.toVal
}

// object Main extends App {
// val x = Var("x")
// val ex = (x + Constant(5)) * x + Constant(11) * x
// ex.toVal(Map("x" -> 2.0))
// implicit val ctx: Map[String, Double] = Map("x" -> 2.0)
// println(ex.toVal)
// }
