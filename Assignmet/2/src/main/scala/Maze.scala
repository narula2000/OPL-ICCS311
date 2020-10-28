// Collaborator: Parm, Chakeera, Tasfia, Krittin

object Maze {
  def solveMaze(maze: List[String]): Option[String] = {
    val rowSize = maze.head.length
    def direction(
        node: List[Int],
        map: Map[Int, Set[Int]],
        nodes: List[Int]
    ): Map[Int, Set[Int]] =
      node match {
        case head :: next => {
          direction(
            next,
            map + (head -> Set(
              head - 1,
              head + 1,
              head - rowSize,
              head + rowSize
            ).filter(r => nodes.contains(r))),
            nodes
          )
        }
        case Nil => map
      }
    val graph = ??? // Make graph
    val solver = ??? // BFS for the answer
    Some("Answer")
  }
}
