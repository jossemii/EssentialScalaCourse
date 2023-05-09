
/**

Demand for Cat Simulator 1.0 is exploding! For v2 we’re going to go beyond
the domes c cat to model Tigers, Lions, and Panthers in addi on to the Cat.
Deﬁne a trait Feline and then deﬁne all the diﬀerent species as subtypes of
Feline. To make things interes ng, deﬁne:
• on Feline a colour as before;
• on Feline a String sound, which for a cat is "meow" and is "roar"
for all other felines;
• only Cat has a favourite food; and
• Lions have an Int maneSize.

 */

trait Callable {
  def name: String
}

sealed trait Feline extends Callable {
  def colour: String
  def sound: String
  def dinner: Food

  def dinner_functional: Food = this match
    case Lion(_, _) => Antelope
    case Tiger(_, _) => TigerFood
    case Panther(_, _) => Licorice
    case Cat(_, _, food) => CatFood(food)
}

sealed trait BigCat extends Feline {
  val sound = "roar"
}

case class Tiger(name: String, colour: String) extends BigCat {
  override val sound = "roar, im a tiger ..."
  override def dinner: Food = TigerFood
}
case class Panther(name: String, colour: String) extends BigCat {
  override def dinner: Food = Licorice
}
case class Lion(name: String, colour: String) extends BigCat {
  override def dinner: Food = Antelope
}
case class Cat(name: String, colour: String, food: String) extends Feline {
  val sound = "meow"

  override def dinner: Food = CatFood(food)
}


sealed trait Food
case object Antelope extends Food
case object TigerFood extends Food
case object Licorice extends Food
final case class CatFood(food: String) extends Food


// Tests

val first_cat: Cat = Cat(name = "Oswald", colour = "Black", food = "Milk")
val second_cat: Cat = Cat(name = "Henderson", colour = "Ginger", food = "Chips")
val third_cat: Cat = Cat(name = "Quentin", colour = "Tabby and white", food = "Curry")

object ChipShop {
  def willServe(cat: Cat): Boolean = cat.food == "Chips"
  def willServeByPatternMatching(cat: Cat): Boolean = cat.food match
    case "Chips" => true
    case _ => false

  def willServeByPatternMatching2(cat: Cat): Boolean = cat match
    case Cat(_, _, "Chips") => true
    case Cat(_, _, _) => false
}


def cats_test(): Unit = {
  println(ChipShop.willServe(first_cat))
  println(ChipShop.willServe(second_cat))
  println(ChipShop.willServe(third_cat))

  println("------")

  println(ChipShop.willServeByPatternMatching(first_cat))
  println(ChipShop.willServeByPatternMatching(second_cat))
  println(ChipShop.willServeByPatternMatching(third_cat))

  println("------")

  println(ChipShop.willServeByPatternMatching2(first_cat))
  println(ChipShop.willServeByPatternMatching2(second_cat))
  println(ChipShop.willServeByPatternMatching2(third_cat))
}