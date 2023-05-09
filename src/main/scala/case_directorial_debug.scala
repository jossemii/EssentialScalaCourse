
case class CaseDirector(firstName: String, lastName: String, yearOfBirth: Int) {
  def name: String = s"$firstName $lastName"
}

object CaseDirector {
  def older(director1: CaseDirector, director2: CaseDirector): CaseDirector = {
    if (director1.yearOfBirth > director2.yearOfBirth) director2
    else director1
  }
}

case class CaseFilm(name: String, yearOfRelease: Int, imbdRating: Double, director: CaseDirector) {
  def directorsAge: Int = yearOfRelease - director.yearOfBirth
  def isDirectedBy(director: CaseDirector): Boolean = director.name == this.director.name
}

object CaseFilm {
  def highestRating(film1: CaseFilm, film2: CaseFilm): CaseFilm = {
    if (film1.imbdRating > film2.imbdRating) film1
    else film2
  }
  def oldestDirectorAtTheTime(film1: CaseFilm, film2: CaseFilm): CaseDirector = {
    if(film1.directorsAge > film2.directorsAge) film1.director
    else film2.director
  }
}

def case_director_test(): Unit = {
  val eastwood = new CaseDirector("Clint", "Eastwood", 1930)
  val mcTiernan = new CaseDirector("John", "McTiernan", 1951)
  val nolan = new CaseDirector("Christopher", "Nolan", 1970)
  val someBody = new CaseDirector("Just", "Some Body", 1990)
  val memento = new CaseFilm("Memento", 2000, 8.5, nolan)
  val darkKnight = new CaseFilm("Dark Knight", 2008, 9.0, nolan)
  val inception = new CaseFilm("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = new CaseFilm("High Plains Drifter", 1973, 7.7,
    eastwood)
  val outlawJoseyWales
  = new CaseFilm("The Outlaw Josey Wales", 1976, 7.9,
    eastwood)
  val unforgiven = new CaseFilm("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new CaseFilm("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new CaseFilm("Invictus", 2009, 7.4, eastwood)
  val predator = new CaseFilm("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new CaseFilm("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new CaseFilm("The Hunt for Red October", 1990,
    7.6, mcTiernan)
  val thomasCrownAffair = new CaseFilm("The Thomas Crown Affair", 1999, 6.8,
    mcTiernan)

  assert(eastwood.yearOfBirth == 1930)
  assert(dieHard.director.name == "John McTiernan")
  assert(!invictus.isDirectedBy(nolan))
}