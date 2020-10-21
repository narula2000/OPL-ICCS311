trait Dessert
case class Pie(kind: String) extends Dessert
case class Smoothie(fruits: List[String]) extends Dessert
case class Cake(toppings: String) extends Dessert

object Liquid extends App {
  def isLiquid(what: Dessert): Boolean = {
    what match {
      case dump: Smoothie => true
      case _              => false
    }
  }
}
