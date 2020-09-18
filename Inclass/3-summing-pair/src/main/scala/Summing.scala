import scala.collection.mutable

object Summing extends App {

  def sumPairList(xs: List[(Int, Int)]): Int  = {
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
    val x1: List[Int] = firsts(xs)
    val x2: List[Int] = seconds(xs)
    var sumX1: Int = 0
    var sumX2: Int = 0
    x1.foreach(x => sumX1 = sumX1 + x)
    x2.foreach(x => sumX2 = sumX2 + x)
    val ans: (Int, Int) = (sumX1, sumX2)
    ans
  }

  val test: List[(Int, Int)] = List((1, 2),(2, 3),(3, 4),(4, 5))

  println(sumPairList(test))
  println(firsts(test))
  println(seconds(test))
  println(pairSumList(test))
}