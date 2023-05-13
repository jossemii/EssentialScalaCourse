import scala.annotation.tailrec

sealed trait IntList {
  @tailrec
  private final def length(list: IntList, total: Int): Int = list match {
    case Pair(head, tail) => length(tail, total + 1)
    case End => total
  }
  def length: Int = this.length(this, 0)

  /**
   * Deﬁne a method to double the value of each element in an IntList, returning
   * a new IntList.
   */
  @tailrec
  private final def double(list: IntList, doubled_list: IntList): IntList = list match {
    case Pair(head, tail) => double(tail, Pair(head*2, tail))
    case End => list
  }
}

case object End extends IntList

final case class Pair(head:Int, tail: IntList) extends IntList

@tailrec
def sum_with_head(list: IntList, total: Int): Int = list match {
  case Pair(head, tail) => sum_with_head(tail, head + total)
  case End => total
}

def sum(list: IntList): Int = list match {
  case Pair(head, tail) => head + sum(tail)
  case End => 0
}

def recursive_test(): Unit = {
  val list = Pair(1, Pair(2, End))
  assert(3 == sum(list))
  assert(3 == sum_with_head(list, 0))

  val example = Pair(1, Pair(2, Pair(3, End)))
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End.length == 0)
}