sealed trait Node
final case class Lead(value: Int) extends Node
final case class Tree(right: Node, left: Node) extends Node
