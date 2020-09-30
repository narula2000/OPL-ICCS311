import scala.collection.immutable.TreeMap

object Roman extends App {

  val romeNum = TreeMap(
    1000 -> "M",
    900 -> "CM",
    500 -> "D",
    400 -> "CD",
    100 -> "C",
    90 -> "XC",
    50 -> "L",
    40 -> "XL",
    10 -> "X",
    9 -> "IX",
    5 -> "V",
    4 -> "IV",
    1 -> "I"
  )

  def parser(value: Any): String = value.toString.replace("Some(", "").replace(")", "")

  def toRoman(n: Int): String = {
    val answer = new StringBuilder("")
    def inner(n: Int): String = {
      if (n >= 1000) {
        answer.append(parser(romeNum.get(1000)))
        inner(n - 1000)
      }
      else if (n >= 900) {
        answer.append(parser(romeNum.get(900)))
        inner(n - 900)
      }
      else if (n >= 500) {
        answer.append(parser(romeNum.get(500)))
        inner(n - 500)
      }
      else if (n >= 400) {
        answer.append(parser(romeNum.get(400)))
        inner(n - 400)
      }
      else if (n >= 100) {
        answer.append(parser(romeNum.get(100)))
        inner(n - 100)
      }
      else if (n >= 90) {
        answer.append(parser(romeNum.get(90)))
        inner(n - 90)
      }
      else if (n >= 50) {
        answer.append(parser(romeNum.get(50)))
        inner(n - 50)
      }
      else if (n >= 40) {
        answer.append(parser(romeNum.get(40)))
        inner(n - 40)
      }
      else if (n >= 10) {
        answer.append(parser(romeNum.get(10)))
        inner(n - 10)
      }
      else if (n >= 9) {
        answer.append(parser(romeNum.get(9)))
        inner(n - 9)
      }
      else if (n >= 5) {
        answer.append(parser(romeNum.get(5)))
        inner(n - 5)
      }
      else if (n >= 4) {
        answer.append(parser(romeNum.get(4)))
        inner(n - 4)
      }
      else if (n >= 1) {
        answer.append(parser(romeNum.get(1)))
        inner(n - 1)
      }
      answer.toString()
    }
    inner(n)
  }

//   println(toRoman(255))
//   println(toRoman(3))
}
