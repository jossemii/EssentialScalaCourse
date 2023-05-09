
class Director(val firstName: String, val lastName: String, val yearOfBirth: Int) {
  def name: String = s"$firstName $lastName"
}

object Director {
  def apply(firstName: String, lastName: String, yearOfBirth: Int): Director = {
    new Director(firstName, lastName, yearOfBirth)
  }
  def older(director1: Director, director2: Director): Director = {
    if (director1.yearOfBirth > director2.yearOfBirth) director2
    else director1
  }
}

class Film(val name: String, val yearOfRelease: Int, val imbdRating: Double, val director: Director) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth
  def isDirectedBy(director: Director): Boolean = director.name == this.director.name
  def copy(
            name: String = this.name,
            yearOfRelease: Int = this.yearOfRelease,
            imbdRating: Double = this.imbdRating,
            director: Director = this.director
          ): Film = new Film(name, yearOfRelease, imbdRating, director)
}

object Film {
  def apply(name: String, yearOfRelease: Int, imbdRating: Double, director: Director): Film = {
    new Film(name, yearOfRelease, imbdRating, director)
  }
  def highestRating(film1: Film, film2: Film): Film = {
    if (film1.imbdRating > film2.imbdRating) film1
    else film2
  }
  def oldestDirectorAtTheTime(film1: Film, film2: Film): Director = {
    if(film1.directorsAge > film2.directorsAge) film1.director
    else film2.director
  }
}

def test = {
  val eastwood = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan = new Director("Christopher", "Nolan", 1970)
  val someBody = new Director("Just", "Some Body", 1990)
  val memento = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
  val inception = new Film("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7,
    eastwood)
  val outlawJoseyWales
  = new Film("The Outlaw Josey Wales", 1976, 7.9,
    eastwood)
  val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new Film("Invictus", 2009, 7.4, eastwood)
  val predator = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990,
    7.6, mcTiernan)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8,
    mcTiernan)

  assert(eastwood.yearOfBirth == 1930)
  assert(dieHard.director.name == "John McTiernan")
  assert(!invictus.isDirectedBy(nolan))
}