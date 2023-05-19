
object seq_utils {
  def seq_min(seq: Seq[Int]): Int = seq.foldLeft(Int.MaxValue)(math.min)

  def unique(seq: Seq[Int]): Seq[Int] =
    seq.foldLeft(Seq.empty[Int]){ (seq: Seq[Int], el: Int) => if seq.contains(el) then seq else el +: seq }

  def reverse(seq: Seq[Int]): Seq[Int] =
    seq.foldLeft(Seq.empty[Int]){ (seq: Seq[Int], el: Int) => el +: seq}

  def map[A, B](seq: Seq[A], f: A => B): Seq[B] = seq.foldRight(Seq.empty[B]){ (elt, seq) => f(elt) +: seq }

  def foldLeft[A, B](seq: Seq[A], zero: B, f: (B, A) => B): B = {
    var result: B = zero
    seq.foreach{ el => result = f(result, el) }
    result
  }
}
