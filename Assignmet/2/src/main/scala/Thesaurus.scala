// Collaborator: Parm, Chakeera, Tasfia, Krittin

import scala.io.Source.fromFile

object Thesaurus {

  val defaultEncoding = "ISO8859-1"

  def load(filename: String): String => Set[String] = {
    val txt = fromFile(filename)(defaultEncoding)
    val line = txt.getLines().toList.tail
    def loop(
        lst: List[String],
        decision: Int,
        from: String,
        synonyms: Set[String],
        map: Map[String, Set[String]]
    ): Map[String, Set[String]] =
      lst match {
        case head :: next if (decision < 1) => {
          val arg = head.split('|').toList
          loop(
            next,
            arg.tail.head.trim.toInt,
            arg.head.trim,
            Set(),
            map + (from -> synonyms)
          )
        }
        case head :: next => {
          val set =
            head.split('|').toList.tail.toSet.map((elm: String) => elm.trim)
          loop(next, decision - 1, from, synonyms ++ set, map)
        }
        case Nil => map
      }
    val map: Map[String, Set[String]] = loop(line, 0, "", Set(), Map())
    (check: String) => map.get(check).fold(Set[String]())(_ ++ Set[String]())
  }

  def linkage(
      thesaurusFile: String
  ): String => String => Option[List[String]] = {
    val neighborThesaurus = load(thesaurusFile)
    (wordA: String) => {
      val shortest = GraphBFS.bfs[String](neighborThesaurus, wordA)
      val edges = shortest._1
      (wordB: String) => {
        def traverse(
            from: String,
            to: String,
            lst: List[String]
        ): Option[List[String]] = {
          if (from.contentEquals(to)) {
            Some(lst)
          } else {
            val edge = edges.get(from).fold("")(_ + "")
            if (!edge.isEmpty) traverse(edge, to, lst :+ edge)
            else None
          }
        }
        traverse(wordB, wordA, List(wordB))
      }
    }
  }
}
