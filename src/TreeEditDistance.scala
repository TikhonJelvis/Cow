object TreeEditDistance {
  def main(args: Array[String]) {
    def x = List(
      Node("a", List(
        Node("c", List()),
        Node("d", List()),
        Node("e", List())
      )),
      Node("b", List()))
    def y = List(
      Node("a", List(
        Node("c", List(
          Node("f", List())
        )),
        Node("d", List())
      )),
      Node("c", List()),
      Node("g", List())
    )

    pprint(editDistance(x, y))
  }

  def editDistance(f: List[Node], g: List[Node]): List[ChangeNode] = {
    (f, g) match {
      case (List(), List()) => List()
      case (_, List()) => (for ((node, i) <- f.zipWithIndex) yield ChangeNode(Del(node, i), List())).toList
      case (List(), _) => (for ((node, i) <- g.zipWithIndex) yield ChangeNode(Add(node, i), List())).toList
      case (_, _) =>
        val deleted = ChangeNode(Del(f.head, 0), List()) :: editDistance(f.tail, g)
        val added = ChangeNode(Add(g.head, 0), List()) :: editDistance(f, g.tail)
        val mod =
          if (f.head.x == g.head.x) ChangeNode(Identity(f.head.x), editDistance(f.head.children, g.head.children))
          else ChangeNode(Mod(f.head.x, g.head.x), editDistance(f.head.children, g.head.children))
        val changed = mod :: editDistance(f.tail, g.tail)
        List(added, deleted, changed).minBy(_.map(_.length(0.9)).sum)
    }
  }

  def pprint(changes: List[ChangeNode], level: Int = 0) {
    for (ChangeNode(c, children) <- changes) {
      c match {
        case Add(x, _) => println("%sadd %s".format("  " * level, x))
        case Del(x, _) => println("%sdel %s".format("  " * level, x))
        case Mod(s, t) => println("%smod %s -> %s".format("  " * level, s, t))
        case Identity(x) => if (children.nonEmpty) println("%sWithin %s:".format("  " * level, x))
      }
      pprint(children, level + 1)
    }
  }
}

case class Node(x: String, children: List[Node])

case class ChangeNode(c: Change, children: List[ChangeNode]) {
  def length(alpha: Double): Double =
    (c match { case _ : Identity => 0; case _ => alpha }) + children.map(_.length(alpha * alpha)).sum
}

sealed trait Change
case class Add(x: Node, pos: Int) extends Change
case class Del(x: Node, pos: Int) extends Change
case class Mod(s: String, t: String) extends Change
case class Identity(s: String) extends Change
