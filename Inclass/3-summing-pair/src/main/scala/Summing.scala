import scala.collection.mutable

object Summing extends App {

  /*
    * In-class exercise without recursion
  */

  def sumList(xs: List[Int]): Int = {
    var sum = 0
    xs.foreach(x => sum += x)
    sum
  }

  def sumPairList(xs: List[(Int, Int)]): Int = {
    var sum: Int = 0
    xs.foreach(x => sum = sum + x._1 + x._2)
    sum
  }

  def firsts(xs: List[(Int, Int)]): List[Int] = {
    var list = new mutable.ListBuffer[Int]()
    xs.foreach(x => list += x._1)
    list.toList
  }

  def seconds(xs: List[(Int, Int)]): List[Int] = {
    var list = new mutable.ListBuffer[Int]()
    xs.foreach(x => list += x._2)
    list.toList
  }

  def pairSumList(xs: List[(Int, Int)]): (Int, Int) = {
    (sumList(firsts(xs)), sumList(seconds(xs)))
  }

  val test: List[(Int, Int)] = List((1, 2), (2, 3), (3, 4), (4, 5))

  println(sumPairList(test) + " == 24")
  println(firsts(test) + " == (1, 2, 3, 4)")
  println(seconds(test) + " == (2, 3, 4, 5)")
  println(pairSumList(test) + " == (10, 14)")
}
