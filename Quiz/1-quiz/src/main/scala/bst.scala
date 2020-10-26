trait Tree
case class Node(left: Tree, key: Int, right: Tree) extends Tree
case object Empty extends Tree

object bst extends App {
  def walkCPS(tree: Tree): List[Int] = {
    def walker(tree: Tree, C: List[Int] => List[Int]): List[Int] =
      tree match {
        case Empty => C(Nil)
        case Node(left, key, right) =>
          walker(
            left,
            leftN => {
              walker(right, rightN => C(leftN ::: (key :: rightN)))
            }
          )
      }
    walker(tree, (f: List[Int]) => f)
  }

  // val tree = Node(
  // Node(Empty, 2, Empty),
  // 5,
  // Node(Node(Empty, 6, Empty), 7, Node(Empty, 9, Empty))
  // )
//
  // println(walkPreorder(tree))
}

