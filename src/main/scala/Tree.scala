sealed abstract class Tree

case class Branch(value: Int, left: Tree, right: Tree) extends Tree

case object Empty extends Tree

object BinaryTree {
  def max(tree: Tree): Int =
    tree match {
      case Branch(v, Empty, Empty) => v
      case Branch(v, Empty, r) => Math.max(v, max(r))
      case Branch(v, l, Empty) => Math.max(v, max(l))
      case Branch(v, l, r) => Math.max(v, Math.max(max(l), max(r)))
      case Empty => throw new RuntimeException()
    }


  def min(tree: Tree): Int =
    tree match {
      case Branch(v, Empty, Empty) => v
      case Branch(v, Empty, r) => Math.min(v, min(r))
      case Branch(v, l, Empty) => Math.min(v, min(l))
      case Branch(v, l, r) => Math.min(v, Math.min(min(l), min(r)))
      case Empty => throw new RuntimeException()
    }

  def depth(tree: Tree): Int =
    tree match {
      case Empty => 0
      case Branch(v, l, r) => Math.max(depth(l), depth(r)) + 1
    }
}