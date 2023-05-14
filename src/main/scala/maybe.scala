sealed trait Maybe[A] {
  def fold[B](full: A => B, empty: B): B = this match
    case Empty() => empty
    case Full(value) => full(value)
}

final case class Empty[A]() extends Maybe[A]
final case class Full[A](value: A) extends Maybe[A]