sealed trait LinkedList[A] {
  def map[B](fn: A => B): LinkedList[B] =
    this match {
      case Pair_map(hd, tl) => Pair_map(fn(hd), tl.map(fn))
      case End() => End[B]()
    }
}
final case class Pair_map[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
final case class End[A]() extends LinkedList[A]