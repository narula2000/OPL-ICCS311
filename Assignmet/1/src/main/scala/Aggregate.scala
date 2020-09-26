import scala.math._

object Aggregate extends App {

  def myMin(p: Double, q: Double, r: Double): Double =
    if (p <= q && p <= r)
      p
    else if (p <= r && p <= q)
      q
    else
      r

  def myMean(p: Double, q: Double, r: Double): Double =
    (p + q + r) / 3

  def myMed(p: Double, q: Double, r: Double): Double =
    if ((p - q) * (q - r) >= 0)
      q
    else if ((p - q) * (p - r) >= 0)
      r
    else
      p

  // println(myMin(1, 9, 3) + " == 1.0")
  // println(myMean(3, 7, 4) + " == 4.666")
  // println(myMed(13, 5.0, 12) + " == 12.0")
}
