
sealed trait Source
case object Well extends Source
case object Spring extends Source
case object Tap extends Source

final case class BottleWater(size: Int, source: Source, carbonated: Boolean)
