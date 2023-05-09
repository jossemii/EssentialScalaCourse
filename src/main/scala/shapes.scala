import scala.math.{Pi, pow}

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

sealed trait Rectangular extends Shape {
  def width: Double
  def height: Double
  val sides = 4
  def perimeter: Double = 2 * (width + height)
  def area: Double = width * height / 2
}

final case class Circle(radius: Double) extends Shape
{
  val sides = 1
  val perimeter: Double = 2 * radius * Pi
  val area: Double = pow(radius, 2) * Pi

  override def toString: String = s"A circle of radius $radius cm"
}

final case class Rectangle(width: Double, height: Double) extends Rectangular
{
  override def toString: String = s"A rectangle of width $width cm and height $height cm"
}

final case class Square(size: Double) extends Rectangular {
  val width: Double = size
  val height: Double = size

  override def toString: String = s"A square of size $size cm"
}

object Draw {
  def test(shape: Shape): String = shape.toString

  def apply(shape: Shape): String = shape match
    case Rectangle(width, height) => s"A rectangle of width $width cm and height $height cm"
    case Square(size) => s"A square of size $size cm"
    case Circle(radius) => s"A circle of radius $radius cm"
}