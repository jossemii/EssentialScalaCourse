class Cat(val name: String, val color: String, val food: String)

val first_cat = new Cat(name = "Oswald", color = "Black", food = "Milk")
val second_cat = new Cat(name = "Henderson", color = "Ginger", food = "Chips")
val third_cat = new Cat(name = "Quentin", color = "Tabby and white", food = "Curry")

object ChipShop {
  def willServe(cat: Cat): Boolean = cat.food == "Chips"
}

def test(): Unit = {
  println(ChipShop.willServe(first_cat))
  println(ChipShop.willServe(second_cat))
  println(ChipShop.willServe(third_cat))
}