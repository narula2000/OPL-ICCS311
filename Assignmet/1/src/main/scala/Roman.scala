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
      answer.append(romeNum.get(1000).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 1000)
    }
    else if (n >= 900) {
      answer.append(romeNum.get(900).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 900)
    }
    else if (n >= 500) {
      answer.append(romeNum.get(500).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 500)
    }
    else if (n >= 400) {
      answer.append(romeNum.get(400).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 400)
    }
    else if (n >= 100) {
      answer.append(romeNum.get(100).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 100)
    }
    else if (n >= 90) {
      answer.append(romeNum.get(90).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 90)
    }
    else if (n >= 50) {
      answer.append(romeNum.get(50).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 50)
    }
    else if (n >= 40) {
      answer.append(romeNum.get(40).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 40)
    }
    else if (n >= 10) {
      answer.append(romeNum.get(10).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 10)
    }
    else if (n >= 9) {
      answer.append(romeNum.get(9).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 9)
    }
    else if (n >= 5) {
      answer.append(romeNum.get(5).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 5)
    }
    else if (n >= 4) {
      answer.append(romeNum.get(4).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 4)
    }
    else if (n >= 1) {
      answer.append(romeNum.get(1).toString().replace("Some(", "").replace(")", ""))
      toRoman(n - 1)
    }
    answer.toString()
  }

  // val test: Int = 3
  // println(toRoman(test))
}
