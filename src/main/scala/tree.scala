sealed trait Tree {
  def sum: Int
  def double: Tree

  def sum_patmat: Int = this match
    case Node(right, left) => right.sum + left.sum
    case Lead(value) => value

  def double_patmat: Tree = this match
    case Node(right, left) => Node(right = right.double, left = left.double)
    case Lead(value) => Lead(2*value)
}

final case class Lead(value: Int) extends Tree {
  def sum: Int = value
  def double: Tree = Lead(2*value)
}

final case class Node(right: Tree, left: Tree) extends Tree {
  def sum: Int = right.sum + left.sum
  def double: Tree = Node(right.double, left.double)
}
