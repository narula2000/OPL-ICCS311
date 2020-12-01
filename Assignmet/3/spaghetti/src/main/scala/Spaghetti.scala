object Spaghetti extends App {

  def readAloud(lst: List[Long]): List[Long] = {
    def loop(
        lst: List[Long],
        added: List[Long],
        accum: Long,
        reading: Long
    ): List[Long] =
      lst match {
        case head :: next if (head == reading) =>
          loop(next, added, accum + 1, reading)
        case head :: next => loop(next, added :+ accum :+ reading, 1, head)
        case Nil          => added :+ accum :+ reading
      }
    loop(lst, List(), 0, lst.head)
  }

  def mergeString(lst: List[Long], buffer: String): String =
    lst match {
      case head :: next => mergeString(next, buffer + head.toString())
      case Nil          => buffer
    }

  def flip(k: String, buffer: String): String =
    k.toList match {
      case head :: next if (head == '0') =>
        flip(next.mkString(""), buffer + '1')
      case head :: next if (head == '1') =>
        flip(next.mkString(""), buffer + '0')
      case Nil => buffer
    }

  def H(n: Int): List[String] = {
    if (n == 0) {
      List("")
    } else {
      List.concat(
        H(n - 1).map(x => '0' + x),
        H(n - 1).map(x => '0' + x).map(b => flip(b, ""))
      )
    }
  }

  def spaghetti: Stream[String] = {
    def loop(lst: List[Long]): Stream[String] = {
      mergeString(lst, "") #:: loop(readAloud(lst))
    }
    loop(List(1))
  }

  def ham: Stream[String] = {
    def loop(number: Int): Stream[String] = {
      Stream.from(H(number)) #::: loop(number + 1)
    }
    loop(1)
  }

  // println(spaghetti.take(8).toList)
  // println("==============================")
  // println(ham.take(8).toList)
}

