case class Cat(name: String, color: String, food: String)

val first_cat: Cat = Cat(name = "Oswald", color = "Black", food = "Milk")
val second_cat: Cat = Cat(name = "Henderson", color = "Ginger", food = "Chips")
val third_cat: Cat = Cat(name = "Quentin", color = "Tabby and white", food = "Curry")

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