import math.Ordered

object mergeSort extends App {
  def mergeSort[T](lst: List[T])(implicit order: T => Ordered[T]): List[T] = {
    def merge(left: List[T], right: List[T]): List[T] = {
      (left, right) match {
        case (Nil, _) => right
        case (_, Nil) => left
        case (leftH :: leftT, rightH :: rightT) =>
          if (leftH < rightH) { leftH :: merge(leftT, right) }
          else {
            rightH :: merge(left, rightT)
          }
      }
    }
    val size = lst.length / 2
    if (size == 0) {
      lst
    } else {
      val (left, right) = lst.splitAt(size)
      merge(mergeSort(left), mergeSort(right))
    }
  }
}
