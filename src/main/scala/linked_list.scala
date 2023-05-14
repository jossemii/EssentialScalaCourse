import scala.annotation.tailrec

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

sealed trait LinkedList[T] {

  final def apply(position: Int): Result[T] = this match
    case Pair(head, tail) => position match
      case 0 => Success(head)
      case _ => tail(position -1)
    case End() => Failure("Index out of bounds")

  @tailrec
  final def contains(total: Int = 0): Int = this match {
    case Pair(head, tail) => tail.contains(total = total +1)
    case End() => total
  }

  /**
   * Deﬁne a method to double the value of each element in an IntList, returning
   * a new IntList.
   */
  /**@tailrec
  final def double(doubled_list: LinkedList[T] = End[T]()): IntList = this match {
    case Pair(head, tail) => tail.double(Pair(head*2, tail))
    case End => doubled_list
  }

  @tailrec
  final def sum(total: Int = 0): Int = this match {
    case Pair(head, tail) => tail.sum(head + total)
    case End[T]() => total
  }*/
}

final case class End[T]() extends LinkedList[T]

final case class Pair[T](head:T, tail: LinkedList[T]) extends LinkedList[T]

def recursive_test(): Unit = {
  val example = Pair(1, Pair(2, Pair(3, End[Int]())))
  assert(example.contains() == 3)

  /**
   * Implement a method apply that returns the nth item in the list
   */
  assert(example(0) == Success(1))
  assert(example(1) == Success(2))
  assert(example(2) == Success(3))
  assert(example(3) == Failure("Index out of bounds"))
}