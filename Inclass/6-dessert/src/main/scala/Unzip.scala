import scala.annotation.tailrec
import scala.collection.mutable

object Unzip extends App {
  def unzip[A, B](xs: List[(A, B)]): (List[A], List[B]) = {
    var a = List()
    var b = List()
    @tailrec
    def inner(xs: List[(A, B)], a: List[A], b: List[B]): (List[A], List[B]) =
      xs match {
        case head :: next =>
          inner(next, a ::: List(head._1), b ::: List(head._2))
        case Nil => (a, b)
      }
    inner(xs, a, b)
  }
}
