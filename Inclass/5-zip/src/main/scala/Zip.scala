object Zip extends App {

  def zip(x: List[Int], y: List[Int]): List[(Int, Int)] =
    x zip y

  def unzip(zipped: List[(Int, Int)]): (List[Int], List[Int]) =
    zipped.unzip

  val testUnzip = List((3, 6), (2, 1), (5, 9))
  val testZip1 = List(3, 2, 5)
  val testZip2 = List(6, 1, 9)

  println(zip(testZip1, testZip2))
  println(unzip(testUnzip))
}
