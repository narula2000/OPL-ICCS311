import scala.collection.mutable
import scala.annotation.tailrec

object ZipRec extends App {

  def zip(xs: List[Int], ys: List[Int]): List[(Int, Int)] =
    (xs, ys) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (x :: xTail, y :: yTail) => (x, y) :: zip(xTail, yTail)
    }

  def unzip(zipped: List[(Int, Int)]): (List[Int], List[Int]) = {
    var a = new mutable.ListBuffer[Int]()
    var b = new mutable.ListBuffer[Int]()
    @tailrec
    def inner(
        xs: List[(Int, Int)],
        a: mutable.ListBuffer[Int],
        b: mutable.ListBuffer[Int]): (List[Int], List[Int]) =
      xs match {
        case head :: next => inner(next, a.append(head._1), b.append(head._2))
        case Nil => (a.toList, b.toList)
      }
    inner(zipped, a, b)
  }

  def unzipConcat(zipped: List[(Int, Int)]): (List[Int], List[Int]) = {
    var a = List()
    var b = List()
    @tailrec
    def inner(
        xs: List[(Int, Int)],
        a: List[Int],
        b: List[Int]): (List[Int], List[Int]) =
      xs match {
        case head :: next => inner(next, a ::: List(head._1), b ::: List(head._2))
        case Nil => (a, b)
      }
    inner(zipped, a, b)
  }

  val testUnzip = List((3, 6), (2, 1), (5, 9))
  val testZip1 = List(3, 2, 5)
  val testZip2 = List(6, 1, 9)

  println(zip(testZip1, testZip2))
  println(unzip(testUnzip))
  println(unzipConcat(testUnzip))
}
