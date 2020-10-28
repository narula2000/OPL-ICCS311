// Collaborator: Parm, Chakeera, Tasfia, Krittin

object ReadAloud extends App {
  def readAloud(lst: List[Int]): List[Int] = {
    def loop(
        lst: List[Int],
        answer: List[Int],
        acc: Int,
        reader: Int
    ): List[Int] =
      lst match {
        case head :: next if (head == reader) =>
          loop(next, answer, acc + 1, reader)
        case head :: next =>
          loop(next, answer :+ acc :+ reader, 1, head)
        case Nil => answer :+ acc :+ reader
      }
    loop(lst, List(), 0, lst.head)
  }
  def unreadAloud(readLst: List[Int]): List[Int] = {
    def loop(
        lst: List[Int],
        answer: List[Int],
        acc: Int,
        reader: Int
    ): List[Int] =
      acc match {
        case 0 =>
          lst match {
            case head :: next =>
              loop(next.tail, answer, head, next.head)
            case Nil => answer
          }
        case _ => loop(lst, answer :+ reader, acc - 1, reader)
      }
    loop(readLst.tail.tail, List(), readLst.head, readLst.tail.head)
  }
}
