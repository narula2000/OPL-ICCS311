import scala.annotation.tailrec

object CountWhile extends App {
  def countWhile[T](xs: List[T], key: T): Int = {
    @tailrec
    def inner(xs: List[T], key: T, counter: Int): Int = {
      xs match {
        case head :: next =>
          if (head == key) {
            inner(next, key, counter + 1)
          } else {
            inner(next, key, counter)
          }
        case Nil => counter
      }
    }
    inner(xs, key, 0)
  }
}
