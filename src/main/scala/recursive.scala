import scala.annotation.tailrec

sealed trait IntList {
  @tailrec
  final def length(total: Int = 0): Int = this match {
    case Pair(head, tail) => tail.length(total + 1)
    case End => total
  }

  /**
   * Deﬁne a method to double the value of each element in an IntList, returning
   * a new IntList.
   */
  @tailrec
  final def double(doubled_list: IntList = End): IntList = this match {
    case Pair(head, tail) => tail.double(Pair(head*2, tail))
    case End => doubled_list
  }

  @tailrec
  final def sum(total: Int = 0): Int = this match {
    case Pair(head, tail) => tail.sum(head + total)
    case End => total
  }
}

case object End extends IntList

final case class Pair(head:Int, tail: IntList) extends IntList

def recursive_test(): Unit = {
  val example = Pair(1, Pair(2, Pair(3, End)))
  assert(example.sum() == 6)
}