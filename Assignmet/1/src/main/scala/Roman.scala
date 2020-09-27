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

  val answer = new StringBuilder("")

  def toRoman(n: Int): String = {
    if (n >= 1000) {
      answer.append(String.valueOf(romeNum.get(1000).toString()))
      toRoman(n - 1000)
    }
    else if (n >= 900) {
      answer.append(String.valueOf(romeNum.get(900).toString()))
      toRoman(n - 900)
    }
    else if (n >= 500) {
      answer.append(String.valueOf(romeNum.get(500).toString()))
      toRoman(n - 500)
    }
    else if (n >= 400) {
      answer.append(String.valueOf(romeNum.get(400).toString()))
      toRoman(n - 400)
    }
    else if (n >= 100) {
      answer.append(String.valueOf(romeNum.get(100).toString()))
      toRoman(n - 100)
    }
    else if (n >= 90) {
      answer.append(String.valueOf(romeNum.get(90).toString()))
      toRoman(n - 90)
    }
    else if (n >= 50) {
      answer.append(String.valueOf(romeNum.get(50).toString()))
      toRoman(n - 50)
    }
    else if (n >= 40) {
      answer.append(String.valueOf(romeNum.get(40).toString()))
      toRoman(n - 40)
    }
    else if (n >= 10) {
      answer.append(String.valueOf(romeNum.get(10).toString()))
      toRoman(n - 10)
    }
    else if (n >= 9) {
      answer.append(String.valueOf(romeNum.get(9).toString()))
      toRoman(n - 9)
    }
    else if (n >= 5) {
      answer.append(String.valueOf(romeNum.get(5).toString()))
      toRoman(n - 5)
    }
    else if (n >= 4) {
      answer.append(String.valueOf(romeNum.get(4).toString()))
      toRoman(n - 4)
    }
    else if (n >= 1) {
      answer.append(String.valueOf(romeNum.get(1).toString()))
      toRoman(n - 1)
    }
    answer.toString()
  }

  val test: Int = 10
  println(toRoman(test))
}
