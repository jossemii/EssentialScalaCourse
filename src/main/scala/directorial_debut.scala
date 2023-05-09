
class NonCaseDirector(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = s"$firstName $lastName"
}

object NonCaseDirector {
  def apply(firstName: String, lastName: String, yearOfBirth: Int): NonCaseDirector = {
    new NonCaseDirector(firstName, lastName, yearOfBirth)
  }
  def older(director1: NonCaseDirector, director2: NonCaseDirector): NonCaseDirector = {
    if (director1.yearOfBirth > director2.yearOfBirth) director2
    else director1
  }
}

class NonCaseFilm(val name: String, val yearOfRelease: Int, val imbdRating: Double, val director: NonCaseDirector) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth
  def isDirectedBy(director: NonCaseDirector): Boolean = director.name == this.director.name
  def copy(
            name: String = this.name,
            yearOfRelease: Int = this.yearOfRelease,
            imbdRating: Double = this.imbdRating,
            director: NonCaseDirector = this.director
          ): NonCaseFilm = new NonCaseFilm(name, yearOfRelease, imbdRating, director)
}

object NonCaseFilm {
  def apply(name: String, yearOfRelease: Int, imbdRating: Double, director: NonCaseDirector): NonCaseFilm = {
    new NonCaseFilm(name, yearOfRelease, imbdRating, director)
  }
  def highestRating(film1: NonCaseFilm, film2: NonCaseFilm): NonCaseFilm = {
    if (film1.imbdRating > film2.imbdRating) film1
    else film2
  }
  def oldestDirectorAtTheTime(film1: NonCaseFilm, film2: NonCaseFilm): NonCaseDirector = {
    if(film1.directorsAge > film2.directorsAge) film1.director
    else film2.director
  }
}

def director_test(): Unit = {
  val eastwood = new NonCaseDirector("Clint", "Eastwood", 1930)
  val mcTiernan = new NonCaseDirector("John", "McTiernan", 1951)
  val nolan = new NonCaseDirector("Christopher", "Nolan", 1970)
  val someBody = new NonCaseDirector("Just", "Some Body", 1990)
  val memento = new NonCaseFilm("Memento", 2000, 8.5, nolan)
  val darkKnight = new NonCaseFilm("Dark Knight", 2008, 9.0, nolan)
  val inception = new NonCaseFilm("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = new NonCaseFilm("High Plains Drifter", 1973, 7.7,
    eastwood)
  val outlawJoseyWales
  = new NonCaseFilm("The Outlaw Josey Wales", 1976, 7.9,
    eastwood)
  val unforgiven = new NonCaseFilm("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new NonCaseFilm("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new NonCaseFilm("Invictus", 2009, 7.4, eastwood)
  val predator = new NonCaseFilm("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new NonCaseFilm("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new NonCaseFilm("The Hunt for Red October", 1990,
    7.6, mcTiernan)
  val thomasCrownAffair = new NonCaseFilm("The Thomas Crown Affair", 1999, 6.8,
    mcTiernan)

  assert(eastwood.yearOfBirth == 1930)
  assert(dieHard.director.name == "John McTiernan")
  assert(!invictus.isDirectedBy(nolan))
}