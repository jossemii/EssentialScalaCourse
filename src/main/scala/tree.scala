sealed trait Tree[A] {

  def fold[B](pair: (B, B) => B, end: A => B): B = this match
    case Node(right, left) => pair(right.fold(pair, end), left.fold(pair, end))
    case Lead(value) => end(value)

  // def sum: A = this.fold[A](A.sum, _)

  // def double: Tree[A] = this.fold[Tree[A]](Node(_, _), Lead(A))
}

final case class Lead[A](value: A) extends Tree[A]
final case class Node[A](right: Tree[A], left: Tree[A]) extends Tree[A]
