import cats.effect.IO
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
import io.circe.generic.auto._
import io.circe.syntax._

object ApiRoutes {

  private implicit val reqDecoder: EntityDecoder[IO, TruthTableReq] =
    jsonOf[IO, TruthTableReq]

  private implicit val respEncoder: EntityEncoder[IO, TruthTableResp] =
    jsonEncoderOf[IO, TruthTableResp]

  private implicit val bddReqDecoder: EntityDecoder[IO, BddReq] =
    jsonOf[IO, BddReq]

  private implicit val bddRespEncoder: EntityEncoder[IO, BddResp] =
    jsonEncoderOf[IO, BddResp]

  private implicit val reduceReqDecoder: EntityDecoder[IO, ReduceTraceReq] =
    jsonOf[IO, ReduceTraceReq]

  private implicit val terminalsTraceRespEncoder: EntityEncoder[IO, ReduceTerminalsTraceResp] =
    jsonEncoderOf[IO, ReduceTerminalsTraceResp]

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root =>
      Ok("OK")

    case req @ POST -> Root / "truth-table" =>
      (for {
        body <- req.as[TruthTableReq]
        expr <- IO.fromEither(
          TempParser.parse(body.expr).left.map(e => new IllegalArgumentException(e.toString))
        )
        table <- IO(BoolExpr.truthTable(expr, body.vars))
        rows = table.map { case (envMap, out) =>
          val envList = body.vars.map(v => envMap(v))
          TruthTableRow(envList, out)
        }
        r <- Ok(TruthTableResp(body.vars, rows).asJson)
      } yield r).handleErrorWith {
        case e: IllegalArgumentException => BadRequest(e.getMessage)
        case other => BadRequest(other.getMessage)
      }

    case req @ POST -> Root / "bdd" =>
      (for {
        body <- req.as[BddReq]
        expr <- IO.fromEither(
          TempParser.parse(body.expr).left.map(e => new IllegalArgumentException(e.toString))
        )
        tt <- IO(BoolExpr.truthTable(expr, body.vars))
        rows = tt.map { case (envMap, out) =>
          BDDFromTruthTable.Row(
            body.vars.map(v => envMap(v)).toVector,
            out
          )
        }.toVector
        root = BDDFromTruthTable.build(body.vars.toVector, rows)
        elements = BDDExport.toCytoscape(root, body.vars.toVector)
        r <- Ok(BddResp(elements).asJson)
      } yield r).handleErrorWith {
        case e: IllegalArgumentException => BadRequest(e.getMessage)
        case other =>
          BadRequest(s"${other.getClass.getName}: ${Option(other.getMessage).getOrElse("")}")
      }

    case req @ POST -> Root / "bdd" / "reduce-terminals-trace" =>
      (for {
        body <- req.as[ReduceTraceReq]
        _ <- IO.println(s"[reduce-merge-trace] applied(back) = ${body.applied}")
        _ <- IO.println(s"[reduce-terminals-trace] applied(back) = ${body.applied}")
        _ <- IO.println(s"[reduce-redundant-trace] applied(back) = ${body.applied}")
        expr <- IO.fromEither(
          TempParser.parse(body.expr).left.map(e => new IllegalArgumentException(e.toString))
        )

        tt <- IO(BoolExpr.truthTable(expr, body.vars))
        rows = tt.map { case (envMap, out) =>
          BDDFromTruthTable.Row(body.vars.map(v => envMap(v)).toVector, out)
        }.toVector

        root0 = BDDFromTruthTable.build(body.vars.toVector, rows)
        rootStart = ReduceSnap.applyAlreadyApplied(root0, body.applied)
        initial = BDDExport.toCytoscape(rootStart, body.vars.toVector)

        steps <- IO {
          ReduceSnap.traceTerminals(rootStart, body.vars.toVector)
        }

        r <- Ok(ReduceTerminalsTraceResp(initial, steps).asJson)
      } yield r).handleErrorWith {
        case e: IllegalArgumentException => BadRequest(e.getMessage)
        case other => BadRequest(other.getMessage)
      }

    case req @ POST -> Root / "bdd" / "reduce-redundant-trace" =>
      (for {
        body <- req.as[ReduceTraceReq]
        _ <- IO.println(s"[reduce-merge-trace] applied(back) = ${body.applied}")
        _ <- IO.println(s"[reduce-terminals-trace] applied(back) = ${body.applied}")
        _ <- IO.println(s"[reduce-redundant-trace] applied(back) = ${body.applied}")
        expr <- IO.fromEither(
          TempParser.parse(body.expr).left.map(e => new IllegalArgumentException(e.toString))
        )

        tt <- IO(BoolExpr.truthTable(expr, body.vars))
        rows = tt.map { case (envMap, out) =>
          BDDFromTruthTable.Row(body.vars.map(v => envMap(v)).toVector, out)
        }.toVector

        root0 = BDDFromTruthTable.build(body.vars.toVector, rows)
        rootStart = ReduceSnap.applyAlreadyApplied(root0, body.applied)
        initial = BDDExport.toCytoscape(rootStart, body.vars.toVector)

        steps <- IO {
          ReduceSnap.traceRedundant(rootStart, body.vars.toVector)
        }

        r <- Ok(ReduceSnapTraceResp(initial, steps).asJson)
      } yield r).handleErrorWith {
        case e: IllegalArgumentException => BadRequest(e.getMessage)
        case other => BadRequest(other.getMessage)
      }

    case req@POST -> Root / "bdd" / "reduce-merge-trace" =>
      (for {
        body <- req.as[ReduceTraceReq]
        _ <- IO.println(s"[reduce-merge-trace] applied(back) = ${body.applied}")
        _ <- IO.println(s"[reduce-terminals-trace] applied(back) = ${body.applied}")
        _ <- IO.println(s"[reduce-redundant-trace] applied(back) = ${body.applied}")
        expr <- IO.fromEither(
          TempParser.parse(body.expr).left.map(e => new IllegalArgumentException(e.toString))
        )

        tt <- IO(BoolExpr.truthTable(expr, body.vars))
        rows = tt.map { case (envMap, out) =>
          BDDFromTruthTable.Row(body.vars.map(v => envMap(v)).toVector, out)
        }.toVector

        root0 = BDDFromTruthTable.build(body.vars.toVector, rows)
        rootStart = ReduceSnap.applyAlreadyApplied(root0, body.applied)
        initial = BDDExport.toCytoscape(rootStart, body.vars.toVector)

        steps <- IO {
          ReduceSnap.traceMerge(rootStart, body.vars.toVector)
        }

        r <- Ok(ReduceSnapTraceResp(initial, steps).asJson)
      } yield r).handleErrorWith {
        case e: IllegalArgumentException => BadRequest(e.getMessage)
        case other => BadRequest(other.getMessage)
      }
  }
}