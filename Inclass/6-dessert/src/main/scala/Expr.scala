trait Expr {
  def +(that: Expr) = {}
  def *(that: Expr) = {}
  def uanry_- = {}
  def toVal(implicit ctx: Map[String, Double]): Double
}

case class Var(name: String) extends Expr {
  override def toVal(implicit ctx: Map[String, Double]): Double =
    name.toString()
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
