import scala.math._
import scala.annotation.tailrec

object Factorial extends App {

  def factorial(n: Int): Long = {
    @tailrec
    def inner(acc: Long, n: Int): Long =
      if (n <= 1) acc
      else inner(n * acc, n - 1)
    inner(1, n)
  }

  // println(factorial(0) + " == 1")
  // println(factorial(10) + " == 3628800")
  // println(factorial(19) + " == 12164510040883200")
}
