import cats.effect._
import cats.syntax.all._
import com.comcast.ip4s._
import fs2.io.file.Path
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server._
import org.http4s.server.Router
import org.http4s.server.staticcontent._

object Main extends IOApp.Simple {

  // Serve files from: <project-root>/frontend
  private val frontendDir: Path = Path("frontend")

  // 1) GET / -> frontend/index.html
  private val indexRoute: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root =>
      StaticFile
        .fromPath(frontendDir / "index.html", Some(req))
        .getOrElseF(NotFound())
  }

  // 2) GET /styles.css, /app.js, etc.
  private val staticFiles: HttpRoutes[IO] =
    fileService[IO](FileService.Config(systemPath = "frontend"))

  // 3) API routes (existing http4s endpoints)
  private val api: HttpRoutes[IO] = ApiRoutes.routes

  private val app: HttpApp[IO] =
    Router(
      "/" -> (indexRoute <+> staticFiles),
      "/api" -> api
    ).orNotFound

  override def run: IO[Unit] =
    EmberServerBuilder.default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(app)
      .build
      .useForever
}
