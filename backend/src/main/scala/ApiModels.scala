// backend/src/main/scala/ApiModels.scala

final case class TruthTableReq(expr: String, vars: List[String])
final case class TruthTableRow(env: List[Boolean], out: Boolean)
final case class TruthTableResp(vars: List[String], rows: List[TruthTableRow])

final case class BddReq(expr: String, vars: List[String])
final case class BddResp(elements: BDDExport.CyElements)

final case class ReduceTraceReq(
                                 expr: String,
                                 vars: List[String],
                                 applied: List[String] = Nil
                               )

// --- NEW: ops-based trace for UI ---
final case class GraphOp(
                          kind: String,
                          ids: List[String],
                          to: Option[String] = None
                        )
final case class TraceSnapStep(
                                title: String,
                                focus: List[String],
                                snapshot: BDDExport.CyElements
                              )

final case class ReduceSnapTraceResp(
                                      initial: BDDExport.CyElements,
                                      steps: List[TraceSnapStep]
                                    )

final case class ReduceTerminalsTraceResp(
                                           initial: BDDExport.CyElements,
                                           steps: List[TraceSnapStep]
                                         )