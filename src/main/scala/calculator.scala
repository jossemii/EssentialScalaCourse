import scala.annotation.targetName

sealed trait Calculation
final case class SuccessCalculation(result: Double) extends Calculation
final case class FailureCalculation(reason: String) extends Calculation


sealed trait Expression {
  private def eval: Calculation = this match

    case Addition(left, right) => (left.eval, right.eval) match
      case (SuccessCalculation(result_left), SuccessCalculation(result_right)) => SuccessCalculation(result_left + result_right)
      case _ => FailureCalculation("Non success")

    case Subtraction(left, right) => (left.eval, right.eval) match
      case (SuccessCalculation(result_left), SuccessCalculation(result_right)) => SuccessCalculation(result_left - result_right)
      case _ => FailureCalculation("Non success")

    case Number(value) => SuccessCalculation(value)

    case Division(left, right) => (left.eval, right.eval) match
      case (SuccessCalculation(result_left), SuccessCalculation(result_right)) => divide(result_left, result_right) match
        case Finite(value) => SuccessCalculation(value)
        case Infinite => FailureCalculation("Infinite")
      case _ => FailureCalculation("Non success")
}
final case class Addition(left: Expression, right: Expression) extends Expression
final case class Subtraction(left: Expression, right: Expression) extends Expression
final case class Number(value: Double) extends Expression
final case class Division(left: Expression, right: Expression) extends Expression


object Calculator {
  @targetName("Sum")
  def +(calculation: Calculation, value: Double): Calculation = calculation match
    case SuccessCalculation(result) => SuccessCalculation(result = result + value)
    case FailureCalculation(reason) => FailureCalculation(reason = reason)

  @targetName("Rest")
  def -(calculation: Calculation, value: Double): Calculation = calculation match
    case SuccessCalculation(result) => SuccessCalculation(result = result - value)
    case FailureCalculation(reason) => FailureCalculation(reason = reason)

  @targetName("Division")
  def /(calculation: Calculation, value: Int): Calculation = calculation match
    case SuccessCalculation(result) => if value == 0 then FailureCalculation("Division by zero") else SuccessCalculation(result = result / value)
    case FailureCalculation(reason) => FailureCalculation(reason = reason)
}


def calculator_test(): Unit = {
  assert(Calculator.+(SuccessCalculation(1), 1) == SuccessCalculation(2))
  assert(Calculator.-(SuccessCalculation(1), 1) == SuccessCalculation(0))
  assert(Calculator.+(FailureCalculation("Badness"), 1) == FailureCalculation("Badness"))

  assert(Calculator./(SuccessCalculation(4), 2) == SuccessCalculation(2))
  assert(Calculator./(SuccessCalculation(4), 0) == FailureCalculation("Division by zero"))
  assert(Calculator./(FailureCalculation("Badness"), 0) == FailureCalculation("Badness"))
}