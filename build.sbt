import scala.scalanative.build._

ThisBuild / version := "0.20"

ThisBuild / scalaVersion := "3.4.2"

lazy val root = (project in file("."))
  .enablePlugins(JavaAppPackaging)
  .enablePlugins(DockerPlugin)
  .settings(
    name              := "killmail-router",
    libraryDependencies ++= List(
      "org.scodec"       %% "scodec-bits"             % "1.1.38",
      "org.http4s"       %% "http4s-ember-server"     % "1.0.0-M41",
      "org.http4s"       %% "http4s-ember-client"     % "1.0.0-M41",
      "org.http4s"       %% "http4s-dsl"              % "1.0.0-M41",
      "org.http4s"       %% "http4s-circe"            % "1.0.0-M41",
      "org.tpolecat"     %% "skunk-core"              % "1.1.0-M3",
      // "com.armanbilge" %% "circe-scala-yaml"        % "0.0.4",
      "io.circe"         %% "circe-yaml"              % "1.15.0", // jvm only and uses snake yaml, but it's less shit than scala-yaml
      "com.monovore"     %% "decline"                 % "2.4.1",
      "org.typelevel"    %% "cats-effect-cps"         % "0.4.0",
      "org.typelevel"    %% "log4cats-core"           % "2.7.0",
      "co.fs2"           %% "fs2-io"                  % "3.10-365636d",
      "co.fs2"           %% "fs2-core"                % "3.10-365636d",
      "io.circe"         %% "circe-literal"           % "0.14.8",
      "org.typelevel"    %% "spire"                   % "0.18.0",
      "net.andimiller"   %% "cats-parse-interpolator" % "0.1.0",
      "net.andimiller"   %% "hedgehogs-core"          % "0.3.0",
      "net.andimiller"   %% "hedgehogs-circe"         % "0.3.0",
      "com.github.cb372" %% "scalacache-caffeine"     % "1.0.0-M6",
      "org.typelevel"    %% "spire-laws"              % "0.18.0" % Test,
      "org.typelevel"    %% "munit-cats-effect"       % "2.0.0-M4" % Test,
      "org.typelevel"    %% "discipline-munit"        % "2.0.0-M3" % Test,
      "org.scalameta"    %% "munit-scalacheck"        % "1.0.0-M11" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-source:future"
    ),
    dockerBaseImage   := "eclipse-temurin:17",
    dockerRepository  := Some("andimiller"),
    dockerExecCommand := Seq("podman"),
    Docker / mappings ++= List(
      "capitals.json",
      "citadels.json",
      "rigsizes.json",
      "systems.json"
    ).map { s => file(s) -> s"/opt/docker/$s" }
  )

lazy val readme = (project in file("docs"))
  .settings(
    (publish / skip) := true,
    mdocVariables    := Map(
      "VERSION" -> version.value
    ),
    mdocIn           := file("./docs/readme.md"),
    mdocOut          := file("./readme.md")
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin)
