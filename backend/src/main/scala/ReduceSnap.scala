object ReduceSnap {
  import BDDCore._
  import BDDExport._
  import scala.collection.mutable

  // --------------------------------------------
  // Utilities (shared by all three steps)
  // --------------------------------------------

  def dfsCollect(root: BDDNode): List[BDDNode] = {
    val seen = mutable.HashSet.empty[BDDNode]
    val out  = mutable.ListBuffer.empty[BDDNode]

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

  /** parents map: child -> ListBuffer[(parent, isLowChild)] */
  def buildParents(
                    root: BDDNode
                  ): mutable.HashMap[BDDNode, mutable.ListBuffer[(NonTerminal, Boolean)]] = {

    val parents =
      mutable.HashMap.empty[BDDNode, mutable.ListBuffer[(NonTerminal, Boolean)]]

    def add(child: BDDNode, p: NonTerminal, isLow: Boolean): Unit = {
      val buf = parents.getOrElseUpdate(child, mutable.ListBuffer.empty)
      buf += ((p, isLow))
    }

    val seen = mutable.HashSet.empty[BDDNode]
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

  def replaceInParents(
                        parents: mutable.HashMap[BDDNode, mutable.ListBuffer[(NonTerminal, Boolean)]],
                        oldNode: BDDNode,
                        newNode: BDDNode
                      ): Unit = {
    parents.get(oldNode).foreach { buf =>
      buf.foreach { case (p, isLow) =>
        if (isLow) p.low = newNode else p.high = newNode
        val b2 = parents.getOrElseUpdate(newNode, mutable.ListBuffer.empty)
        b2 += ((p, isLow))
      }
    }
    parents.remove(oldNode)
  }

  /** Semantic key: terminals by value, non-terminals by identity. */
  def sk(n: BDDNode): (String, Int) = n match {
    case Terminal(v, _, _) => ("T", if (v) 1 else 0)
    case _                 => ("N", System.identityHashCode(n))
  }

  // --------------------------------------------
  // Apply already-applied steps to get rootStart
  // --------------------------------------------

  def applyAlreadyApplied(root0: BDDNode, applied: List[String]): BDDNode = {
    var root = root0

    applied.foreach {
      case "terminals" => root = applyTerminals(root)
      case "redundant" => root = applyRedundant(root)
      case "merge" => root = applyMerge(root)
      case _ => ()
    }

    root
  }

  // --------------------------------------------
  // Step 1: terminals (apply only, no snapshots)
  // --------------------------------------------

  private def applyTerminals(root0: BDDNode): BDDNode = {
    var root = root0

    val terms = dfsCollect(root).collect { case t: Terminal => t }
    if (terms.isEmpty) return root

    val parents = buildParents(root)

    val falseRep = terms.find(!_.value).getOrElse(terms.head)
    val trueRep  = terms.find(_.value).getOrElse(terms.head)

    terms.foreach { t =>
      val canon = if (t.value) trueRep else falseRep
      if (!(t eq canon)) {
        if (root eq t) root = canon
        replaceInParents(parents, t, canon)
      }
    }

    root
  }

  // --------------------------------------------
  // Step 2: redundant tests (apply only, no snapshots)
  // --------------------------------------------

  private def applyRedundant(root0: BDDNode): BDDNode = {
    var root = root0
    var changed = true

    // Iterate until no more redundant nodes exist.
    while (changed) {
      changed = false
      val parents = buildParents(root)

      // semantic redundant: low and high are equivalent (including terminals by value)
      val redundant = dfsCollect(root).collect {
        case nt: NonTerminal if sk(nt.low) == sk(nt.high) => nt
      }

      if (redundant.nonEmpty) {
        redundant.foreach { nt =>
          val child = nt.low
          if (root eq nt) root = child
          replaceInParents(parents, nt, child)
        }
        changed = true
      }
    }

    root
  }

  // --------------------------------------------
  // Step 3: merge non-terminals (apply only, no snapshots)
  // --------------------------------------------

  private def applyMerge(root0: BDDNode): BDDNode = {
    var root = root0
    var changed = true

    while (changed) {
      changed = false
      val parents = buildParents(root)

      val nts = dfsCollect(root).collect { case nt: NonTerminal => nt }
      val levels = nts.map(_.index).distinct.sorted

      levels.foreach { level =>
        val layer = nts.filter(_.index == level)
        if (layer.length > 1) {
          val groups =
            layer.groupBy(nt => (sk(nt.low), sk(nt.high))).values.filter(_.size >= 2).toList

          groups.foreach { g =>
            val keep = g.head
            val dups = g.tail
            if (dups.nonEmpty) {
              dups.foreach { d =>
                if (root eq d) root = keep
                replaceInParents(parents, d, keep)
              }
              changed = true
            }
          }
        }
      }
    }

    root
  }

  // --------------------------------------------
  // Snapshot trace builders (what API returns)
  // --------------------------------------------

  def traceTerminals(rootStart: BDDNode, vars: Vector[String]): List[TraceSnapStep] = {
    var root = rootStart
    val out = mutable.ListBuffer.empty[TraceSnapStep]

    def collectByValue(r: BDDNode, value: Boolean): List[Terminal] =
      dfsCollect(r).collect { case t: Terminal if t.value == value => t }

    def canonicalizeOneValue(r0: BDDNode, value: Boolean): BDDNode = {
      var r = r0
      val ts = collectByValue(r, value)
      if (ts.length <= 1) return r

      val parents = buildParents(r)
      val rep = ts.head
      ts.tail.foreach { t =>
        if (r eq t) r = rep
        replaceInParents(parents, t, rep)
      }
      r
    }

    val zerosBefore = collectByValue(root, value = false)
    if (zerosBefore.length > 1) {
      root = canonicalizeOneValue(root, value = false)
      out += TraceSnapStep(
        title = "Reduce terminals: merge 0 duplicates",
        focus = zerosBefore.map(BDDExport.cyIdOf),
        snapshot = BDDExport.toCytoscape(root, vars)
      )
    }

    val onesBefore = collectByValue(root, value = true)
    if (onesBefore.length > 1) {
      root = canonicalizeOneValue(root, value = true)
      out += TraceSnapStep(
        title = "Reduce terminals: merge 1 duplicates",
        focus = onesBefore.map(BDDExport.cyIdOf),
        snapshot = BDDExport.toCytoscape(root, vars)
      )
    }

    out.toList
  }

  def traceRedundant(rootStart: BDDNode, vars: Vector[String]): List[TraceSnapStep] = {
    var root = rootStart
    val outSteps = mutable.ListBuffer.empty[TraceSnapStep]

    var changed = true
    var iter = 0
    val maxIters = 256

    while (changed && iter < maxIters) {
      iter += 1
      changed = false

      val parents = buildParents(root)

      val redundant = dfsCollect(root).collect {
        case nt: NonTerminal if sk(nt.low) == sk(nt.high) => nt
      }

      if (redundant.nonEmpty) {
        val focusIds = redundant.map(BDDExport.cyIdOf)

        redundant.foreach { nt =>
          val child = nt.low
          if (root eq nt) root = child
          replaceInParents(parents, nt, child)
        }

        outSteps += TraceSnapStep(
          title = s"Reduce redundant tests: removed ${redundant.length} node(s)",
          focus = focusIds,
          snapshot = BDDExport.toCytoscape(root, vars)
        )

        changed = true
      }
    }

    outSteps.toList
  }

  def traceMerge(rootStart: BDDNode, vars: Vector[String]): List[TraceSnapStep] = {
    var root = rootStart
    val outSteps = mutable.ListBuffer.empty[TraceSnapStep]

    var changed = true
    var iter = 0
    val maxIters = 128

    while (changed && iter < maxIters) {
      iter += 1
      changed = false

      val parents = buildParents(root)
      val nts = dfsCollect(root).collect { case nt: NonTerminal => nt }
      val levels = nts.map(_.index).distinct.sorted

      levels.foreach { level =>
        val layer = nts.filter(_.index == level)
        if (layer.length > 1) {
          val groups =
            layer.groupBy(nt => (sk(nt.low), sk(nt.high))).values.filter(_.size >= 2).toList

          if (groups.nonEmpty) {
            val focusBuf = mutable.ListBuffer.empty[String]
            var mergedCount = 0

            groups.foreach { g =>
              val keep = g.head
              val dups = g.tail
              if (dups.nonEmpty) {
                focusBuf += BDDExport.cyIdOf(keep)
                dups.foreach(d => focusBuf += BDDExport.cyIdOf(d))
                mergedCount += dups.length

                dups.foreach { d =>
                  if (root eq d) root = keep
                  replaceInParents(parents, d, keep)
                }

                changed = true
              }
            }

            if (mergedCount > 0) {
              outSteps += TraceSnapStep(
                title = s"Merge non-terminals: level $level merged $mergedCount node(s)",
                focus = focusBuf.toList.distinct,
                snapshot = BDDExport.toCytoscape(root, vars)
              )
            }
          }
        }
      }
    }

    outSteps.toList
  }
}