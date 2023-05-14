sealed trait LinkedList[A] {
  def map[B](fn: A => B): LinkedList[B] =
    this match {
      case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
      case End() => End[B]()
    }
}
final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
final case class End[A]() extends LinkedList[A]