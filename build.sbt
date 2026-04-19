ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "obdd-backend",
    Compile / unmanagedSourceDirectories += baseDirectory.value / "backend" / "src" / "main" / "scala",
    Compile / unmanagedResourceDirectories += baseDirectory.value / "backend" / "src" / "main" / "resources",
    Compile / mainClass := Some("Main"),
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % "0.23.30",
      "org.http4s" %% "http4s-dsl"          % "0.23.30",
      "org.http4s" %% "http4s-circe"        % "0.23.30",
      "io.circe"   %% "circe-core"          % "0.14.10",
      "io.circe"   %% "circe-generic"       % "0.14.10",
      "io.circe"   %% "circe-parser"        % "0.14.10"
    )
  )
Compile / run / fork := true
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.16"
ThisBuild / scalacOptions += "-Xmax-inlines:128"
assembly / assemblyJarName := s"${name.value}-assembly-${version.value}.jar"
assembly / mainClass := Some("Main")
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case PathList("META-INF", "INDEX.LIST") => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) if xs.exists(_.endsWith(".SF")) => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) if xs.exists(_.endsWith(".DSA")) => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) if xs.exists(_.endsWith(".RSA")) => MergeStrategy.discard
  case PathList("META-INF", xs @ _*) => MergeStrategy.deduplicate
  case "NOTICE" => MergeStrategy.discard
  case "LICENSE" => MergeStrategy.discard
  case "module-info.class" => MergeStrategy.discard
  case _ => MergeStrategy.deduplicate
}
