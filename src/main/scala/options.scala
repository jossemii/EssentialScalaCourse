class options {
  def addOptions(option_a: Option[Int], option_b: Option[Int]): Option[Int] =
    for {
      a <- option_a
      b <- option_b
    } yield a + b

  def addOptions_with_map(option_a: Option[Int], option_b: Option[Int]): Option[Int] =
    option_a.flatMap(a => option_b.map(b => a + b))

  def add3Options(option_a: Option[Int], option_b: Option[Int], option_c: Option[Int]): Option[Int] =
    for {
      a <- option_a
      b <- option_b
      c <- option_c
    } yield a + b + c

  def add3Options_with_map(option_a: Option[Int], option_b: Option[Int], option_c: Option[Int]): Option[Int] =
    option_a.flatMap(a => option_b.flatMap(b => option_c.map(c => a + b + c)))

  def divide(a: Int, b: Int): Option[Int] = if b > 0 then Some(a / b) else None

  def divideOptions(option_a: Option[Int], option_b: Option[Int]) =
    for {
      a <- option_a
      b <- option_b
      c <- divide(a, b)
    } yield c


  private def parseOperandFromString(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

  private def parseOperatorFromString(s: String): Option[(Int, Int) => Option[Int]] = s match
    case "+" => Some((a: Int, b: Int) => Some(a + b))
    case "-" => Some((a: Int, b: Int) => Some(a - b))
    case "*" => Some((a: Int, b: Int) => Some(a * b))
    case "/" => Some((a: Int, b: Int) => if b > 0 then Some(a / b) else None)
    case _ => None

  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    for {
      a <- parseOperandFromString(operand1)
      op <- parseOperatorFromString(operator)
      b <- parseOperandFromString(operand2)
    } print(
        op(a, b) match
          case Some(number) => print(s"The answer is $number!")
          case None         => print(s"Error calculating $operand1 $operator $operand2")
      )
  }

  def calculator_with_map(operand1: String, operator: String, operand2: String): Unit = {
    parseOperandFromString(operand1).flatMap(
      a => parseOperatorFromString(operator).flatMap(
        op => parseOperandFromString(operand2).map(
          b => op(a, b)
        )
      )
    ) match
      case Some(number) => print(s"The answer is $number!")
      case None => print(s"Error calculating $operand1 $operator $operand2")
  }

}
