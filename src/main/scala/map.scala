sealed trait LinkedList_map[A] {
  def map[B](fn: A => B): LinkedList_map[B] =
    this match {
      case Pair_map(hd, tl) => Pair_map(fn(hd), tl.map(fn))
      case End_map() => End_map[B]()
    }
}
final case class Pair_map[A](head: A, tail: LinkedList_map[A]) extends LinkedList_map[A]
final case class End_map[A]() extends LinkedList_map[A]