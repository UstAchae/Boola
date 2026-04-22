  // backend/src/main/scala/BDDFromTruthTable.scala
object BDDFromTruthTable {
  import BDDCore._

  final case class Row(env: Vector[Boolean], out: Boolean)

  def build(vars: Vector[String], rows: Vector[Row]): BDDNode = {
    val n = vars.length
    val leafCount = 1 << n

    def go(level: Int, rs: Vector[Row], l: Int, r: Int): BDDNode = {
      if (level >= n) {
        val v = rs.headOption.map(_.out).getOrElse(false)
        Terminal(value = v, uid = s"T:${if (v) 1 else 0}:$l:$r")
      } else {
        val (hi, lo) = rs.partition(_.env(level))
        val mid = (l + r) / 2
        val lowNode  = go(level + 1, lo, l, mid)
        val highNode = go(level + 1, hi, mid, r)
        NonTerminal(
          index = level + 1,
          low = lowNode,
          high = highNode,
          uid = s"N:${level + 1}:$l:$r"
        )
      }
    }

    go(level = 0, rs = rows, l = 0, r = leafCount)
  }
}