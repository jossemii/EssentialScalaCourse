import scala.annotation.targetName
import scala.math.{Pi, pow}

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
  def color: Color
}

sealed trait Rectangular extends Shape {
  def width: Double

  def height: Double

  val sides = 4

  def perimeter: Double = 2 * (width + height)

  def area: Double = width * height / 2
}

final case class Rectangle(width: Double, height: Double, color: Color) extends Rectangular
{
  override def toString: String = s"A rectangle of width $width cm and height $height cm"
}

final case class Square(size: Double, color: Color) extends Rectangular {
  val width: Double = size
  val height: Double = size

  override def toString: String = s"A square of size $size cm"
}

final case class Circle(radius: Double, color: Color) extends Shape
{
  val sides = 1
  val perimeter: Double = 2 * radius * Pi
  val area: Double = pow(radius, 2) * Pi

  override def toString: String = s"A circle of radius $radius cm"
}

object Draw {
  def apply(shape: Shape): String = shape match {
    case Circle(radius, color) =>
      s"A ${Draw(color)} circle of radius ${radius}cm"
    case Square(size, color) =>
      s"A ${Draw(color)} square of size ${size}cm"
    case Rectangle(width, height, color) =>
      s"A ${Draw(color)} rectangle of width ${width}cm and height ${
        height}cm"
  }
  def apply(color: Color): String = color match {
    // We deal with each of the predefined Colors with special cases:
    case Red => "red"
    case Yellow => "yellow"
    case Pink=> "pink"
    case color=> if(color.isLight) "light" else "dark"
  }
}


/**
Write a sealed trait Color to make our shapes more interest ng.
• give Color three proper es for its RGB values;
• create three predeﬁned colours: Red, Yellow, and Pink;
• provide a means for people to produce their own custom Colors with
their own RGB values;
• provide a means for people to tell whether any Color is “light” or “dark”.
*/

case class Max255(value: Int) {
  def apply: Int = if value > 255 then 255 else if value < 0 then 0 else value

  @targetName("Sumatory")
  def +(x: Max255): Max255 = Max255(value = value + x.value)
  @targetName("Compare >")
  def >(x: Int): Boolean = value > x
}

sealed trait Color {
  def red: Max255
  def green: Max255
  def blue: Max255

  private def rgb_total: Max255 = red + green + blue
  def isLight: Boolean = rgb_total > 383

}

case object Red extends Color {
  val red: Max255 = Max255(255)
  val green: Max255 = Max255(0)
  val blue: Max255 = Max255(0)
}

case object Yellow extends Color {
  val red: Max255 = Max255(255)
  val green: Max255 = Max255(255)
  val blue: Max255 = Max255(255)
}

case object Pink extends Color {
  val red: Max255 = Max255(255)
  val green: Max255 = Max255(102)
  val blue: Max255 = Max255(255)
}

final case class CustomColor(
                              red: Max255,
                              green: Max255,
                              blue: Max255) extends Color