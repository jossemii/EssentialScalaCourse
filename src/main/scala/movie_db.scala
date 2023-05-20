object movie_db {
  case class Film(
                   name: String,
                   yearOfRelease: Int,
                   imdbRating: Double)

  case class Director(
                       firstName: String,
                       lastName: String,
                       yearOfBirth: Int,
                       films: Seq[Film]){
    def name: String = s"${this.firstName} ${this.lastName}"
  }

  val memento = new Film("Memento", 2000, 8.5)
  val darkKnight = new Film("Dark Knight", 2008, 9.0)
  val inception = new Film("Inception", 2010, 8.8)
  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9)
  val unforgiven = new Film("Unforgiven", 1992, 8.3)
  val granTorino = new Film("Gran Torino", 2008, 8.2)
  val invictus = new Film("Invictus", 2009, 7.4)
  val predator = new Film("Predator", 1987, 7.9)
  val dieHard = new Film("Die Hard", 1988, 8.3)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990,
    7.6)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8)

  val eastwood = new Director("Clint", "Eastwood", 1930,
    Seq(highPlainsDrifter, outlawJoseyWales, unforgiven, granTorino,
      invictus))
  val mcTiernan = new Director("John", "McTiernan", 1951,
    Seq(predator, dieHard, huntForRedOctober, thomasCrownAffair))
  val nolan = new Director("Christopher", "Nolan", 1970, Seq(memento, darkKnight, inception))
  val someGuy = new Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  def directorWithXFilms(numberOfFilms: Int): Seq[Director] = directors.filter(_.films.size > numberOfFilms)

  def directorBornBefore(year: Int): Seq[Director] = directors.filter(_.yearOfBirth < year)

  def directorBornBeforeWithBackCatalogOfSize(year: Int, numberOfFilms: Int): Seq[Director] =
    directors
      .filter(_.films.size > numberOfFilms)
      .filter(_.yearOfBirth < year)

  def directorsSortedByAge(ascending: Boolean = true): Seq[Director] =
    val comparator: (Director, Director) => Boolean =
      if(ascending) {
        (a, b) => a.yearOfBirth < b.yearOfBirth
      } else {
        (a, b) => a.yearOfBirth > b.yearOfBirth
      }
    directors.sortWith(comparator)

  def film_names_of_director(director: Director = nolan): Seq[String] = for {
    film <- director.films
  } yield film.name

  def all_film_names: Seq[String] = for {
    director <- directors
    film <- director.films
  } yield film.name

  def films_sorted_by_IMDB: Seq[Film] = (
      for {
        director <- directors
        film <- director.films
      } yield film
    ).sortWith( (a, b) => a.imdbRating > b.imdbRating )

  def tonight_printer: Unit = for {
    director <- directors
    film <- director.films
  } println(s"Tonight! ${film.name} by ${director.name}!")

}
