import scala.math.pow

object Happy extends App {

  def sumOfDigitsSquared(n: Int) = {
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
    var happy: Int = 0
    var iter: Int = 0
    while (happy != k) {
      iter += 1
      if (isHappy(iter))
        happy += 1
    }
    iter
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
