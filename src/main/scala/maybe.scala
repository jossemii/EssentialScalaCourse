object maybe{
  sealed trait Maybe_[+A] {
    def fold[B](full: A => B, empty: B): B = this match
      case Empty_ => empty
      case Full_(value) => full(value)
  }

  case object Empty_ extends Maybe_[Nothing]
  final case class Full_[A](value: A) extends Maybe_[A]
}