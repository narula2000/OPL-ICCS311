import scala.math.pow

object Happy extends App {

  def sumOfDigitsSquared(n: Int): Int = {
    var ans: Int = 0
    n.toString().map(x => ans += pow(x.asDigit, 2).toInt)
    ans
  }

  def isHappy(n: Int): Boolean =
    n match {
      case 1 => true
      case 4 => false
      case _ => isHappy(sumOfDigitsSquared(n))
    }

  def kThHappy(k: Int): Int = {
    def inner(k: Int, happy: Int, iter: Int): Int =
      if (k != happy)
        if (isHappy(iter)) inner(k, happy + 1, iter + 1)
        else inner(k, happy, iter + 1)
      else iter - 1
    inner(k, 0, 1)
  }

  // println(sumOfDigitsSquared(145) + " == 42")
  // println(sumOfDigitsSquared(7) + " == 49")
  // println(sumOfDigitsSquared(199) + " == 163")
  // println(isHappy(100) + " == T")
  // println(isHappy(111) + " == F")
  // println(isHappy(1234) + " == F")
  // println(isHappy(989) + " == T")
  // println(kThHappy(1) + " == 1")
  // println(kThHappy(3) + " == 10")
  // println(kThHappy(11) + " == 49")
  // println(kThHappy(19) + " == 97")
}
