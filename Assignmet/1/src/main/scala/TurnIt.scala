object TurnIt extends App {

  def transpose(A: List[List[Int]]): List[List[Int]] =
    if (A.head.isEmpty)
      Nil
    else
      A.map(_.head) :: transpose(A.map(_.tail))

  val mtx: List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
  // println(transpose(mtx))
  // println("Should be ->")
  // println("List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))")
}
