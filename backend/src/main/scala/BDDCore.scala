/**
 * BDDCore
 * ├── data structures
 * ├── utils
 * ├── reduceTerminalsIndependently
 * ├── reduceRedundantTestsIndependently
 * ├── reduceMergeNonTerminalsIndependently
 * ├── reduceWithThreeStepTrace     ← UI animation
 * └── reduce                       ← Bryant's reduction algorithm
 */

// backend/src/main/scala/BDDCore.scala
object BDDCore {

  /** Data Structure */
  sealed trait BDDNode {
    var mark: Boolean
    var id: Int
  }
  final case class Terminal(value: Boolean, var id: Int = 0, var mark: Boolean = false) extends BDDNode
  final case class NonTerminal(
                                index: Int, // 1..n
                                var low: BDDNode,
                                var high: BDDNode,
                                var id: Int = 0,
                                var mark: Boolean = false
                              ) extends BDDNode

  // ---------------------------
  // Three-step UI trace (independent steps) + snapshots
  // ---------------------------

  final case class ThreeStepTraceStep(
                                       title: String,
                                       focus: List[BDDNode],
                                       snapshotRoot: BDDNode
                                     )

  private def dfsCollect(root: BDDNode): List[BDDNode] = {
    val seen = scala.collection.mutable.HashSet.empty[BDDNode]
    val out  = scala.collection.mutable.ListBuffer.empty[BDDNode]

    def go(v: BDDNode): Unit = {
      if (!seen.contains(v)) {
        seen += v
        out += v
        v match {
          case NonTerminal(_, lo, hi, _, _) =>
            go(lo); go(hi)
          case Terminal(_, _, _) => ()
        }
      }
    }

    go(root)
    out.toList
  }

  /** parents map: child -> list of (parent, isLowChild) */
  private def buildParents(
                            root: BDDNode
                          ): scala.collection.mutable.HashMap[BDDNode, scala.collection.mutable.ListBuffer[(NonTerminal, Boolean)]] = {

    val parents =
      scala.collection.mutable.HashMap.empty[BDDNode, scala.collection.mutable.ListBuffer[(NonTerminal, Boolean)]]

    def add(child: BDDNode, p: NonTerminal, isLow: Boolean): Unit = {
      val buf = parents.getOrElseUpdate(child, scala.collection.mutable.ListBuffer.empty)
      buf += ((p, isLow))
    }

    val seen = scala.collection.mutable.HashSet.empty[BDDNode]
    def go(v: BDDNode): Unit = {
      if (!seen.contains(v)) {
        seen += v
        v match {
          case p @ NonTerminal(_, lo, hi, _, _) =>
            add(lo, p, true)
            add(hi, p, false)
            go(lo); go(hi)
          case Terminal(_, _, _) => ()
        }
      }
    }

    go(root)
    parents
  }

  private def replaceInParents(
                                parents: scala.collection.mutable.HashMap[BDDNode, scala.collection.mutable.ListBuffer[(NonTerminal, Boolean)]],
                                oldNode: BDDNode,
                                newNode: BDDNode
                              ): Unit = {
    parents.get(oldNode).foreach { buf =>
      buf.foreach { case (p, isLow) =>
        if (isLow) p.low = newNode else p.high = newNode
        val b2 = parents.getOrElseUpdate(newNode, scala.collection.mutable.ListBuffer.empty)
        b2 += ((p, isLow))
      }
    }
    parents.remove(oldNode)
  }

  /** Step 1: canonicalize terminals (force exactly two Terminal nodes used everywhere). */
  def reduceTerminalsIndependently(root0: BDDNode, onStep: ThreeStepTraceStep => Unit): BDDNode = {
    var root = root0

    val nodes = dfsCollect(root)
    val terms = nodes.collect { case t: Terminal => t }

    if (terms.isEmpty) {
      onStep(ThreeStepTraceStep("Reduce terminals: nothing to do", Nil, root))
      root
    } else {
      onStep(ThreeStepTraceStep("Reduce terminals: highlight all terminals", terms, root))

      val parents = buildParents(root)

      val falseRep = terms.find(!_.value).getOrElse(terms.head)
      val trueRep = terms.find(_.value).getOrElse(terms.head)

      terms.foreach { t =>
        val canon = if (t.value) trueRep else falseRep
        if (!(t eq canon)) {
          if (root eq t) root = canon
          replaceInParents(parents, t, canon)
        }
      }

      onStep(ThreeStepTraceStep("Reduce terminals: rewired to canonical 0/1", List(falseRep, trueRep), root))
      root
    }
  }

  /** Step 2: remove redundant tests (u.low == u.high) independently. */
  def reduceRedundantTestsIndependently(root0: BDDNode, onStep: ThreeStepTraceStep => Unit): BDDNode = {
    var root = root0
    val parents = buildParents(root)

    var changed = true
    while (changed) {
      changed = false

      val nodes = dfsCollect(root)
      val redundant = nodes.collect { case nt: NonTerminal if nt.low eq nt.high => nt }

      if (redundant.nonEmpty) {
        onStep(ThreeStepTraceStep("Reduce redundant tests: highlight (low == high)", redundant, root))
      }

      redundant.foreach { nt =>
        val child = nt.low
        if (root eq nt) root = child
        replaceInParents(parents, nt, child)
        changed = true
      }

      if (redundant.nonEmpty) {
        onStep(ThreeStepTraceStep("Reduce redundant tests: removed redundant nodes", redundant, root))
      }
    }

    root
  }

  /** Step 3: merge isomorphic non-terminals independently (group by (index, lowRef, highRef)). */
  def reduceMergeNonTerminalsIndependently(root0: BDDNode, onStep: ThreeStepTraceStep => Unit): BDDNode = {
    var root = root0
    val parents = buildParents(root)

    val levels =
      dfsCollect(root).collect { case nt: NonTerminal => nt.index }.distinct.sorted

    levels.foreach { level =>
      val layer =
        dfsCollect(root).collect { case nt: NonTerminal if nt.index == level => nt }

      if (layer.length > 1) {
        // group by (low identity, high identity)
        val groups =
          layer.groupBy(nt => (nt.low, nt.high)).values.filter(_.size >= 2).toList

        if (groups.nonEmpty) {
          onStep(ThreeStepTraceStep(s"Merge non-terminals: level $level candidates", groups.flatten, root))
        }

        groups.foreach { g =>
          val keep = g.head
          val dups = g.tail

          onStep(ThreeStepTraceStep(s"Merge non-terminals: merge batch at level $level", keep :: dups, root))

          dups.foreach { d =>
            if (root eq d) root = keep
            replaceInParents(parents, d, keep)
          }

          onStep(ThreeStepTraceStep(s"Merge non-terminals: merged batch at level $level", keep :: dups, root))
        }
      }
    }

    root
  }

  /** Unified trace that matches your 3-step UI buttons (each step independent). */
  def reduceWithThreeStepTrace(
                                root: BDDNode,
                                onStep: ThreeStepTraceStep => Unit
                              ): BDDNode = {
    var cur = root
    onStep(ThreeStepTraceStep("Start: original BDD", List(cur), cur))

    cur = reduceTerminalsIndependently(cur, onStep)
    cur = reduceRedundantTestsIndependently(cur, onStep)
    cur = reduceMergeNonTerminalsIndependently(cur, onStep)

    onStep(ThreeStepTraceStep("Finish: reduced BDD", List(cur), cur))
    cur
  }

  // ---------------------------
  // Bryant reduction utilities (required by reduce(...))
  // ---------------------------

  trait Visitor {
    def onEnter(v: BDDNode): Unit

    def onExit(v: BDDNode): Unit = ()
  }

  def traverse(root: BDDNode, visitor: Visitor): Unit = {
    def go(v: BDDNode): Unit = {
      v.mark = !v.mark
      visitor.onEnter(v)

      v match {
        case NonTerminal(_, low, high, _, _) =>
          if (v.mark != low.mark) go(low)
          if (v.mark != high.mark) go(high)
        case Terminal(_, _, _) => ()
      }

      visitor.onExit(v)
    }

    go(root)
  }

  /** Key used for canonicalization within a level. */
  private sealed trait Key

  private final case class TKey(value: Boolean) extends Key

  private final case class NKey(lowId: Int, highId: Int) extends Key

  /** Collect all nodes reachable from root, bucketed by index. Terminals go to vlist(n+1). */
  def bucketByLevel(root: BDDNode, n: Int): Array[List[BDDNode]] = {
    val vlist = Array.fill(n + 2)(List.empty[BDDNode])

    traverse(root, new Visitor {
      override def onEnter(v: BDDNode): Unit = {
        val idx = v match {
          case Terminal(_, _, _) => n + 1
          case NonTerminal(i, _, _, _, _) => i
        }
        vlist(idx) = v :: vlist(idx)
      }
    })

    vlist
  }

  /** Initialize ids for terminals: two unique ids for false/true. */
  def initTerminalIds(
                       vlist: Array[List[BDDNode]],
                       n: Int,
                       subgraph: scala.collection.mutable.ArrayBuffer[BDDNode]
                     ): Int = {
    // reserve: 1 -> False terminal, 2 -> True terminal
    val falseT = Terminal(false, id = 1)
    val trueT = Terminal(true, id = 2)

    // We'll keep index 0 unused to match paper's 1..|G|
    subgraph.clear()
    subgraph += Terminal(false, id = 0) // dummy at index 0
    subgraph += falseT
    subgraph += trueT

    // Assign ids to all terminal occurrences in the original graph
    vlist(n + 1).foreach {
      case t: Terminal =>
        t.id = if (t.value) trueT.id else falseT.id
      case _ => ()
    }

    2 // nextId after inserting 2 terminals
  }

  /** Build key for a node at level i after children ids are ready. */
  private def buildKey(u: BDDNode): Option[Key] = u match {
    case Terminal(v, _, _) => Some(TKey(v))
    case nt: NonTerminal =>
      val lowId = nt.low.id
      val highId = nt.high.id
      // Redundant test: if low and high collapse to same reduced node, this node can be removed.
      if (lowId == highId) None else Some(NKey(lowId, highId))
  }

  /** Process one level i (1..n) and assign reduced ids, update low/high pointers to reduced representatives. */
  def processLevel(
                    i: Int,
                    vlist: Array[List[BDDNode]],
                    subgraph: scala.collection.mutable.ArrayBuffer[BDDNode],
                    nextId0: Int,
                    onStep: (String, List[BDDNode]) => Unit = (_, _) => ()
                  ): Int = {

    val Q = scala.collection.mutable.ArrayBuffer.empty[(Key, NonTerminal)]

    // Step 1: build keys (or mark redundant nodes)
    val redundant = scala.collection.mutable.ListBuffer.empty[BDDNode]
    vlist(i).foreach {
      case nt: NonTerminal =>
        buildKey(nt) match {
          case None =>
            nt.id = nt.low.id
            redundant += nt
          case Some(k) =>
            Q += ((k, nt))
        }
      case _ => ()
    }

    if (redundant.nonEmpty) {
      onStep(s"Level $i: redundant (low == high)", redundant.toList)
    }

    def keyOrder(a: Key, b: Key): Boolean = (a, b) match {
      case (TKey(x), TKey(y)) => (if (x) 1 else 0) < (if (y) 1 else 0)
      case (TKey(_), NKey(_, _)) => true
      case (NKey(_, _), TKey(_)) => false
      case (NKey(al, ah), NKey(bl, bh)) => if (al != bl) al < bl else ah < bh
    }

    val sorted = Q.sortWith { case ((ka, _), (kb, _)) => keyOrder(ka, kb) }

    // Step 2: show isomorphic candidates by grouping equal keys
    val candidates =
      sorted.groupBy(_._1).values.filter(_.size >= 2).flatten.map(_._2).toList
    if (candidates.nonEmpty) {
      onStep(s"Level $i: isomorphic candidates", candidates)
    }

    // Step 3: scan sorted list, assign ids and canonicalize
    var nextId = nextId0
    var oldKey: Option[Key] = None
    var repId: Int = -1

    val mergedAway = scala.collection.mutable.ListBuffer.empty[BDDNode]

    sorted.foreach { case (key, u) =>
      oldKey match {
        case Some(ok) if ok == key =>
          // Share representative id within this key group
          u.id = repId
          mergedAway += u

        case _ =>
          nextId += 1
          u.id = nextId
          repId = u.id

          // Relink children to reduced representatives
          u.low = subgraph(u.low.id)
          u.high = subgraph(u.high.id)

          // Store representative
          subgraph += u
          oldKey = Some(key)
      }
    }

    if (mergedAway.nonEmpty) {
      onStep(s"Level $i: merge isomorphic", mergedAway.toList)
    }

    // Relink step is conceptually done when unique reps are stored; we still emit a frame
    onStep(
      s"Level $i: relink",
      vlist(i).collect { case nt: NonTerminal => nt }
    )
    nextId
  }
}
