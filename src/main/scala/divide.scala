import scala.annotation.targetName

sealed trait DivisionResult
final case class Finite(value: Double) extends DivisionResult
case object Infinite extends DivisionResult

object divide {
  def apply(x: Double, y: Double): DivisionResult = y match
    case 0 => Infinite
    case _ => Finite(value = x/y)
}

def division_test(): Unit = {
  assert(divide(1, 1) == Finite(1))
  assert(divide(1, 0) == Infinite)
}
