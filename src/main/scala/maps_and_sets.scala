object maps_and_sets {
  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred"
  )

  val ages = Map(
    "Alice"-> 20,
    "Bob"-> 30,
    "Charlie" -> 50,
    "Derek"-> 40,
    "Edith"-> 10,
    "Fred"-> 60
  )

  val favoriteColors = Map(
    "Bob"-> "green",
    "Derek"-> "magenta",
    "Fred"-> "yellow"
  )

  val favoriteLolcats = Map(
    "Alice" -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith" -> "Cloud Cat"
  )

  def favoriteColor(person: String): String = favoriteColors.getOrElse(person, "default color")

  def printColors: Unit = for {person <- people} println(favoriteColors(person))

  def lookup[A](name: String, map: Map[String, A]): Any = map.get(name)

  val favorite: Option[String] =
    for {
      oldest <- people.foldLeft(Option.empty[String]) {
        (older, person) =>
          if (ages.getOrElse(person, 0) > older.flatMap(ages.get).getOrElse
          (0)) {
            Some(person)
          } else {
            older
          }
      }
      color <- favoriteColors.get(oldest)
    } yield color

}
