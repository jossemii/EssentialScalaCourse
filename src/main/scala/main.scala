object calc {
  def square(number: Double): Double = number * number
  def cube(number: Double): Double = number * square(number)

  def square(number: Int): Int = number * number

  def cube(number: Int): Int = number * square(number)
}

@main
def main(): Unit = {
  println(
    "Hello world."
  )
}