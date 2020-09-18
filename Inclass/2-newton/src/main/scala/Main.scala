import scala.math._

object Main extends App {
  /*
    * Arjan code fro computing euler number e
  */

  def goodEnough(guess: Double): Boolean = (math.abs(math.log(guess) - 1)) < 1e-10

  def improve(guess: Double): Double = guess - (math.log(guess) - 1)*guess

  def repeat(guess: Double): Double =  {
      if (goodEnough(guess)) guess
      else repeat(improve(guess))
  }

  def computeE = repeat(1.0)

  print("computeE = " + computeE + "\n")
  print("actualE  = " + "2.7182818284590452" + "\n")
}