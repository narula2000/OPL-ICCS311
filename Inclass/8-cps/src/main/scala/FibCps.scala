trait Tree
case class Node(left: Tree, key: Int, right: Tree) extends Tree
case object Empty extends Tree

object FibCps extends App {
  def fib(n: Int) = {
    def fibCps(n: Int, C: Int => Int): Int =
      n match {
        case 0 => C(0)
        case 1 => C(1)
        case _ => fibCps(n - 1, (m: Int) => fibCps(n - 2, (o: Int) => C(m + o)))
      }
    fibCps(n, ((x: Int) => x))
  }

  def walkPreorder(tree: Tree): List[Int] = {
    def walker(tree: Tree, C: List[Int] => List[Int]): List[Int] =
      tree match {
        case Empty => C(Nil)
        case Node(left, key, right) =>
          walker(
            left,
            leftList => {
              walker(right, rightList => C(key :: (leftList ::: rightList)))
            }
          )
      }
    walker(tree, (r: List[Int]) => r)
  }

  // val tree = Node(
  // Node(Empty, 2, Empty),
  // 5,
  // Node(Node(Empty, 6, Empty), 7, Node(Empty, 9, Empty))
  // )
//
  // println(walkPreorder(tree))
}
