sealed trait Maybe_[A] {
  def fold[B](full: A => B, empty: B): B = this match
    case Empty_() => empty
    case Full_(value) => full(value)
}

final case class Empty_[A]() extends Maybe_[A]
final case class Full_[A](value: A) extends Maybe_[A]