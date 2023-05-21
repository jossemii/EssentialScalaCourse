object text_generator {
  val subjects = List("Noel", "The cat", "The dog")
  val verbs = Map(
        "Noel" -> List("wrote", "chased", "slept on"),
        "The cat" -> List("meowed at", "chased", "slept on"),
        "THe dog" -> List("barked at", "chased", "slept on")
    )
  val objects = Map(
    "wrote" -> List("the book", "the letter", "the code"),
    "chased" -> List("the ball", "the dog", "the cat"),
    "slept on" -> List("the bet", "the mat", "the train"),
    "meowed at" -> List("Noel", "the door", "the food cupboard"),
    "barked at" -> List("the postman", "the car", "the cat")
  )

  val sentences: List[String] = for {
    s <- subjects
    v <- verbs(s)
    o <- objects(v)
  } yield s"$s $v $o"

  final case class Distribution[A](events: List[(A, Double)]) {
    def map[B](f: A => B): Distribution[B] = Distribution(events map { case (a, p) => f(a) -> p })

    def flatMap[B](d: Distribution[B])(f: A => Distribution[B]): Distribution[B] =
      Distribution(events flatMap { case (a, p1) =>
        f(a).events map { case (b, p2) => b -> (p1 * p2) }
      }).compact.normalize

    def normalize: Distribution[A] = {
      val totalWeight = (events map { case (a, p) => p }).sum
      Distribution(events map { case (a, p) => a -> (p / totalWeight) })
    }

    def compact: Distribution[A] = {
      val distinct = (events map { case (a, p) => a }).distinct

      def prob(a: A): Double =
        (events filter { case (x, p) => x == a } map { case (a, p) => p
        }).sum

      Distribution(distinct map { a => a -> prob(a) })
    }

  }

  object Distribution {
    def uniform[A](atoms: List[A]): Distribution[A] = {
      val p = 1.0 / atoms.length
      Distribution(atoms.map(a => a -> p))
    }
  }

  sealed trait Coin

  case object Heads extends Coin

  case object Tails extends Coin

  val fairCoin: Distribution[Coin] = Distribution.uniform(List(Heads,
    Tails))

}
