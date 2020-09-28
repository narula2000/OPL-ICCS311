import scala.collection.mutable

object Zombies extends App {

  var ans: Int = 0

  def countBad(hs: List[Int]): Int = {
    mergeSort(hs)
    val badPair: Int = ans
    ans = 0
    badPair
  }

  def merge(left: List[Int], right: List[Int]): List[Int] =
    (left, right) match {
      case (_, Nil) => left
      case (Nil, _) => right
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) {
          ans += left.length
          rightHead :: merge(left, rightTail)
        }
        else {
          leftHead :: merge(leftTail, right)
        }
    }

  def mergeSort(list: List[Int]): List[Int] = {
    val n = list.length / 2
    if (n == 0) list
    else {
      val (left, right) = list.splitAt(n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  // println(countBad(List(35, 22, 10)) + " == 0")
  // println(countBad(List(3, 1, 4, 2)) + " == 3")
  // println(countBad(List(5, 4, 11, 7)) + " == 4")
  // println(countBad(List(1, 3, 22, 13, 25, 4, 10, 34, 16, 28, 19, 31)) + " == 50")
}
