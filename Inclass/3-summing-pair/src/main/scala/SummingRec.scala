import scala.annotation.tailrec
import scala.collection.mutable

object SummingRec extends App {
  /*
    * In-class with recursion
    * Got recursion code from Stack-overflow: https://stackoverflow.com/questions/12496959/summing-values-in-a-list
  */

  def sumList(xs: List[Int]): Int = {
    var sum = 0
    xs.foreach(x => sum += x)
    sum
  }

  def sumPairList(xs: List[(Int, Int)]): Int = {
    @tailrec
    def inner(xs: List[(Int, Int)], sum: Int): Int =
      xs match {
        case head :: tail => inner(tail, sum + head._1 + head._2)
        case Nil => sum
      }
    inner(xs, 0)
  }

  def firsts(xs: List[(Int, Int)]): List[Int] = {
    var list = new mutable.ListBuffer[Int]()
    @tailrec
    def inner(
        xs: List[(Int, Int)],
        sum: mutable.ListBuffer[Int]): mutable.ListBuffer[Int] =
      xs match {
        case head :: tail => inner(tail, sum += head._1)
        case Nil => sum
      }
    inner(xs, list).toList
  }

  def seconds(xs: List[(Int, Int)]): List[Int] = {
    var list = new mutable.ListBuffer[Int]()
    @tailrec
    def inner(
        xs: List[(Int, Int)],
        sum: mutable.ListBuffer[Int]): mutable.ListBuffer[Int] =
      xs match {
        case head :: tail => inner(tail, sum += head._2)
        case Nil => sum
      }
    inner(xs, list).toList
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
