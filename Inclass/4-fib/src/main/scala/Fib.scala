import scala.collection.mutable
import scala.annotation.tailrec

object Fib extends App {

  @tailrec
  def find(xs: List[(Int, String)], key: Int): Option[String] =
    xs match {
      case head :: tail =>
        if (head._1 == key) Some(head._2)
        else find(tail, key)
      case Nil => None
    }

  def rev(xs: List[Int]): List[Int] = {
    @tailrec
    def revRec(xs: List[Int], list: mutable.ListBuffer[Int]): List[Int] =
      xs match {
        case head :: next => revRec(next, list.prepend(head))
        case Nil => list.toList
      }

    var list = new mutable.ListBuffer[Int]()
    revRec(xs, list)
  }

  def revConcat(xs: List[Int]): List[Int] = {
    @tailrec
    def revRec(xs: List[Int], list: List[Int]): List[Int] =
      xs match {
        case head :: next => revRec(next, List(head) ::: list)
        case Nil => list
      }

    var list = List()
    revRec(xs, list)
  }

  def fib(n: Int): Long = {
    @tailrec
    def fibTail(n: Int, first: Int, second: Int): Int =
      n match {
        case 0 => first
        case _ => fibTail(n - 1, second, first + second)
      }
    return fibTail(n, 0, 1)
  }

  val testFind: List[(Int, String)] = List((1, "1"), (2, "2"), (3, "3"))
  val testRev: List[Int] = List(1, 2, 3, 4, 5, 6)

  println(find(testFind, 1) + " == 1")
  println(find(testFind, 2) + " == 2")
  println(find(testFind, 3) + " == 3")
  println(find(testFind, 4) + " == None")
  //println(rev(testRev) + " == List(6, 5, 4, 3, 2, 1)")
  println(revConcat(testRev) + " == List(6, 5, 4, 3, 2, 1)")
  println(fib(8) + " == 21")
}
