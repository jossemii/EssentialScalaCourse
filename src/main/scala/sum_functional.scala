object sum_functional {
  sealed trait Sum[+A, +B] {
    def fold[C](error: A => C, success: B => C): C = this match
      case Failure(a) => error(a)
      case Success(b) => success(b)

    def map[C](f: B => C): Sum[A, C] = this match
      case Failure(value) => Failure(value)
      case Success(value) => Success(f(value))

    def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] = this match
      case Failure(value) => Failure(value)
      case Success(value) => f(value)
  }

  final case class Failure[A, Nothing](value: A) extends Sum[A, Nothing]

  final case class Success[Nothing, B](value: B) extends Sum[Nothing, B]
}