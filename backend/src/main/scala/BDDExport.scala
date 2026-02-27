object BDDExport {
  import BDDCore._

  final case class CyPos(x: Double, y: Double)

  final case class CyNodeData(id: String, label: String)
  final case class CyEdgeData(id: String, source: String, target: String, label: String)

  final case class CyNode(
                           data: CyNodeData,
                           classes: Option[String] = None,
                           position: CyPos
                         )
  final case class CyEdge(data: CyEdgeData, classes: Option[String] = None)

  final case class CyElements(nodes: List[CyNode], edges: List[CyEdge])

  def cyIdOf(v: BDDNode): String =
    "u" + System.identityHashCode(v).toHexString

  def toCytoscape(root: BDDNode, vars: Vector[String]): CyElements = {
    val nodes = scala.collection.mutable.ListBuffer[CyNode]()
    val edges = scala.collection.mutable.ListBuffer[CyEdge]()
    val seen = scala.collection.mutable.Set[String]()

    val expanded = scala.collection.mutable.Set[String]()
    val seenEdges = scala.collection.mutable.Set[String]()

    // ---------- deterministic tree positions ----------
    val pos = scala.collection.mutable.Map[String, CyPos]()

    val n = vars.length
    val yGap = 90.0
    val xGap = 60.0

    // number of nodes = 2^n: use [0, 2^n)
    val leafCount: Double = math.pow(2.0, n.toDouble)

    def assign(v: BDDNode, level0: Int, l: Double, r: Double): Unit = {
      val id = cyIdOf(v)
      if (!pos.contains(id)) {
        val x = ((l + r) / 2.0) * xGap
        val y = level0.toDouble * yGap
        pos(id) = CyPos(x, y)
      }

      v match {
        case NonTerminal(_, low, high, _, _) =>
          val mid = (l + r) / 2.0
          // low always at left hand side, high on the other side
          assign(low, level0 + 1, l, mid)
          assign(high, level0 + 1, mid, r)
        case Terminal(_, _, _) => ()
      }
    }

    def ensureNode(v: BDDNode): String = {
      val id = cyIdOf(v)
      if (!seen.contains(id)) {
        seen += id
        val p = pos.getOrElse(id, CyPos(0.0, 0.0))
        v match {
          case Terminal(value, _, _) =>
            nodes += CyNode(
              data = CyNodeData(id, if (value) "1" else "0"),
              classes = Some("terminal"),
              position = p
            )
          case NonTerminal(index, _, _, _, _) =>
            val name =
              if (index >= 1 && index <= vars.length) vars(index - 1)
              else s"x$index"
            nodes += CyNode(
              data = CyNodeData(id, name),
              classes = None,
              position = p
            )
        }
      }
      id
    }

    def addEdge(src: String, dst: String, lab: String, cls: String): Unit = {
      val id = s"e_${src}_${lab}_${dst}"
      if (!seenEdges.contains(id)) {
        seenEdges += id
        edges += CyEdge(
          data = CyEdgeData(id, src, dst, lab),
          classes = Some(cls)
        )
      }
    }

    def go(v: BDDNode): Unit = {
      val vid = cyIdOf(v)
      if (expanded.contains(vid)) return
      expanded += vid

      v match {
        case NonTerminal(_, low, high, _, _) =>
          val from = ensureNode(v)
          val loId = ensureNode(low)
          val hiId = ensureNode(high)

          addEdge(from, loId, "0", "zero")
          addEdge(from, hiId, "1", "one")

          go(low)
          go(high)

        case _ => ()
      }
    }

    assign(root, level0 = 0, l = 0.0, r = leafCount)

    go(root)
    CyElements(nodes.toList, edges.toList)
  }
}
