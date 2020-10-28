// Collaborator: Parm, Chakeera, Tasfia, Krittin

object GraphBFS {

  def bfs[V](nbrs: V => Set[V], src: V): (Map[V, V], Map[V, Int]) = {
    def expand(
        frontier: Set[V],
        parent: Map[V, V],
        visited: Set[V]
    ): (Set[V], Map[V, V]) = {
      def loop(
          frontier: Set[V],
          parent: Map[V, V],
          Frontier: Set[V],
          Parents: Map[V, V],
          visited: Set[V]
      ): (Set[V], Map[V, V]) = {
        if (frontier.isEmpty) (Frontier, Parents ++ parent)
        else {
          val neighborValue = nbrs(frontier.head)
          loop(
            frontier.tail,
            parent,
            (Frontier ++ neighborValue) -- visited,
            loop2(neighborValue, frontier.head, Parents),
            visited + frontier.head
          )
        }
      }
      def loop2(frontier: Set[V], src: V, Parents: Map[V, V]): Map[V, V] = {
        if (frontier.isEmpty) Parents
        else {
          loop2(frontier.tail, src, Parents + (frontier.head -> src))
        }
      }
      loop(frontier, parent, Set(), Map(), visited)
    }
    def iterate(
        frontier: Set[V],
        parent: Map[V, V],
        distance: Map[V, Int],
        d: Int,
        visited: Set[V]
    ): (Map[V, V], Map[V, Int]) =
      if (frontier.isEmpty)
        (parent, distance + (src -> 0))
      else {
        def mapper(
            frontier: Set[V],
            d: Int,
            distance: Map[V, Int]
        ): Map[V, Int] = {
          if (frontier.isEmpty) distance
          else {
            mapper(frontier.tail, d, distance + (frontier.head -> d))
          }
        }
        val pair = expand(frontier, parent, visited)
        val Distance = distance ++ mapper(pair._1, d + 1, Map())
        iterate(pair._1, pair._2, Distance, d + 1, visited ++ frontier)
      }
    iterate(Set(src), Map(src -> src), Map(), 0, Set())
  }

}
