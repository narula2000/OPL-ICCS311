import scala.annotation.tailrec
import scala.collection.mutable

object SummingRec extends App {
    /*
      * Got recursion code from Stack-overflow: https://stackoverflow.com/questions/12496959/summing-values-in-a-list
    */

    def sumPairList(xs: List[(Int, Int)]): Int = {
        @tailrec
        def inner(xs: List[(Int, Int)], accum: Int): Int = {
        xs match {
            case x :: tail => inner(tail, accum + x._1 + x._2)
            case Nil => accum
        }
        }
        inner(xs, 0)
    }

    def firsts(xs: List[(Int, Int)]): List[Int] = {
        var list = new mutable.ListBuffer[Int]()
        @tailrec
        def inner(xs: List[(Int, Int)], accum: mutable.ListBuffer[Int]): mutable.ListBuffer[Int] = {
            xs match {
            case x :: tail => inner(tail, accum += x._1)
            case Nil => accum
            }
        }
        inner(xs, list).toList
    }

    def seconds(xs: List[(Int, Int)]): List[Int] = {
        var list = new mutable.ListBuffer[Int]()
        @tailrec
        def inner(xs: List[(Int, Int)], accum: mutable.ListBuffer[Int]): mutable.ListBuffer[Int] = {
            xs match {
            case x :: tail => inner(tail, accum += x._2)
            case Nil => accum
            }
        }
        inner(xs, list).toList
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