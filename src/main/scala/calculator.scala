import scala.annotation.targetName

sealed trait Calculation
final case class Success(result: Int) extends Calculation
final case class Failure(reason: String) extends Calculation

object Calculator {
  @targetName("Sum")
  def +(calculation: Calculation, value: Int): Calculation = calculation match
    case Success(result) => Success(result = result + value)
    case Failure(reason) => Failure(reason = reason)

  @targetName("Rest")
  def -(calculation: Calculation, value: Int): Calculation = calculation match
    case Success(result) => Success(result = result - value)
    case Failure(reason) => Failure(reason = reason)

  @targetName("Division")
  def /(calculation: Calculation, value: Int): Calculation = calculation match
    case Success(result) => if value == 0 then Failure("Division by zero") else Success(result = result / value)
    case Failure(reason) => Failure(reason = reason)
}


def calculator_test(): Unit = {
  assert(Calculator.+(Success(1), 1) == Success(2))
  assert(Calculator.-(Success(1), 1) == Success(0))
  assert(Calculator.+(Failure("Badness"), 1) == Failure("Badness"))

  assert(Calculator./(Success(4), 2) == Success(2))
  assert(Calculator./(Success(4), 0) == Failure("Division by zero"))
  assert(Calculator./(Failure("Badness"), 0) == Failure("Badness"))
}