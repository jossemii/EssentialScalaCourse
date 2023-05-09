sealed trait TrafficLight {
  def next_oo: TrafficLight
  def next_f: TrafficLight = this match  // Preferred solution
    case RedLight => GreenLight
    case GreenLight => YellowLight
    case YellowLight => RedLight
}

object NextTraficLight {
  def next(trafficLight: TrafficLight): TrafficLight = trafficLight match
    case RedLight => GreenLight
    case GreenLight => YellowLight
    case YellowLight => RedLight
}

case object RedLight extends TrafficLight {
  override def next_oo: TrafficLight = GreenLight
}
case object YellowLight extends TrafficLight {
  override def next_oo: TrafficLight = RedLight
}
case object GreenLight extends TrafficLight {
  override def next_oo: TrafficLight = YellowLight
}
