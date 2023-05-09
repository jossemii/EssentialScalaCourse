import scala.math.{Pi, pow}

trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

trait Rectangular extends Shape {
  def width: Double
  def height: Double
  val sides = 4
  def perimeter: Double = 2 * (width + height)
  def area: Double = width * height / 2
}

case class Circle(radius: Double) extends Shape
{
  val sides = 1
  val perimeter: Double = 2 * radius * Pi
  val area: Double = pow(radius, 2) * Pi
}

case class Rectangle(val width: Double, val height: Double) extends Rectangular

case class Square(size: Double) extends Rectangular {
  val width: Double = size
  val height: Double = size
}