import scala.math._
import scala.language.postfixOps

object Sqrt extends App {
  /*
    * Newton method for computing square root using Babylonian method
    * Got code and math from Github, Stack-overflow and Wikipedia
    * Github: https://gist.github.com/pushkarnk/9e9b71755a1e22586e55c46b8dbf263e
    * Stack-overflow: https://stackoverflow.com/questions/11106886/scala-doubles-and-precision
    * Wikipedia: https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method
  */

  def roundAt(p: Int)(n: Double): Double = {
    val s = math.pow(10, p);
    (math.round(n * s)) / s
  }

  def roundAt5(n: Double) = roundAt(5)(n)

  def isGoodEnough(guess: Double, y: Double) = math.abs(guess * guess - y)/y < 1e-10

  def improved(guess: Double, y: Double) = (guess + y/guess)/2

  def newtonsMethod(guess: Double, y: Double): Double =  {
      if (isGoodEnough(guess,y)) guess
      else newtonsMethod(improved(guess, y), y)
  }

  def sqrt(y: Double): Double  =  newtonsMethod(1, y)

  // Testing
  print("sqrt(1) = " + roundAt5(sqrt(1)) + "\n")
  print("sqrt(2) = " + roundAt5(sqrt(2)) + "\n")
  print("sqrt(4) = " + roundAt5(sqrt(4)) + "\n")
  print("sqrt(144) = " + roundAt5(sqrt(144)) + "\n")
  print("sqrt(15625) = " + roundAt5(sqrt(15625)) + "\n")
}