class BinaryTree[T <: AnyVal](val rootNode: Node) extends Tree {
  def walkCPS(): List[Int] = {
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
    walker(this.rootNode, (f: List[Int]) => f)
  }
}

trait Tree
case class Node(left: Tree, key: Int, right: Tree) extends Tree
case object Empty extends Tree

object Main extends App {
  /* val tree = new BinaryTree[Int](
    Node(
      Node(Empty, 2, Empty),
      5,
      Node(Node(Empty, 6, Empty), 7, Node(Empty, 8, Empty))
    )
  )
  println(tree.walkCPS()) */

  /* val tree = new BinaryTree[Int](
    Node(
      Node(Node(Empty, 2, Empty), 3, Node(Empty, 4, Empty)),
      5,
      Node(Node(Empty, 6, Empty), 7, Node(Empty, 8, Empty))
    )
  )
  println(tree.walkCPS()) */

  /* val tree = new BinaryTree[Int](
    Node(
      Node(Node(Empty, 2, Empty), 3, Node(Empty, 4, Empty)),
      5,
      Node(Empty, 6, Empty)
    )
  )
  println(tree.walkCPS()) */

  /* val tree = new BinaryTree[Int](
    Node(
      Node(Node(Empty, 2, Empty), 3, Empty),
      4,
      Node(Empty, 5, Node(Empty, 6, Empty))
    )
  )
  println(tree.walkCPS()) */
}
