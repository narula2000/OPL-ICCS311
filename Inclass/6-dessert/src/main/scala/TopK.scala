import scala.collection.mutable.HashMap

object TopK extends App {
  def topK(xs: List[Int], k: Int): List[Int] = {
    val temp = HashMap[Int, Int]().withDefaultValue(0)
    xs.foreach(x => temp(x) += 1)
    List(temp.toSeq.sortWith(_._2 > _._2): _*).slice(0, k).map(_._1).toList
  }
}
