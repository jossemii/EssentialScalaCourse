sealed trait Maybe[A] {
  def flatMap[B](fn: A => Maybe[B]): Maybe[B] = this match
    case Full(v) => fn(v)
    case Empty() => Empty[B]()
}
final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]