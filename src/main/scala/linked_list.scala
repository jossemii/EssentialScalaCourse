import scala.annotation.tailrec

sealed trait Result[A]
case class Success[A](result: A) extends Result[A]
case class Failure[A](reason: String) extends Result[A]

def fold[T](end: T, f: (list: LinkedList[T], total: T) => T): T = this match
  case End => end
  case Pair(head, tail) => f(tail, tail.fold(end, f))

sealed trait LinkedList[T] {

  final def apply(position: Int): Result[T] = this match
    case Pair(head, tail) => position match
      case 0 => Success(head)
      case _ => tail(position -1)
    case End() => Failure("Index out of bounds")


  final def contains: Int = fold[Int](0, (_, total: Int) => total +1)
  // final def sum: T = fold[T](End[T](), (_, total: T, f: (T, T) => T) => f(2, total))

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