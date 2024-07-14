import scala.scalanative.build._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val inDocker = sys.env.get("DOCKER_BUILD").isDefined

lazy val root = (project in file("."))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := "killmail-router",
    nativeConfig ~= { c =>
      c.withLTO(LTO.none)
        .withMode(Mode.debug) // releaseFast might be better for releases
        .withGC(GC.immix)
    },
    libraryDependencies ++= List(
      "org.scodec"     %%% "scodec-bits"         % "1.1.38",
      "org.http4s"     %%% "http4s-ember-server" % "1.0.0-M41",
      "org.http4s"     %%% "http4s-ember-client" % "1.0.0-M41",
      "org.http4s"     %%% "http4s-dsl"          % "1.0.0-M41",
      "org.http4s"     %%% "http4s-circe"        % "1.0.0-M41",
      "org.tpolecat"   %%% "skunk-core"          % "1.1.0-M3",
      "com.armanbilge" %%% "circe-scala-yaml"    % "0.0.4",
      "com.monovore"   %%% "decline"             % "2.4.1",
      "org.typelevel"  %%% "cats-effect-cps"     % "0.4.0",
      "org.typelevel"  %%% "log4cats-core"       % "2.7.0",
      "co.fs2"         %%% "fs2-io"              % "3.10-365636d",
      "co.fs2"         %%% "fs2-core"            % "3.10-365636d",
      "io.circe"       %%% "circe-literal"       % "0.14.8",
      "org.typelevel"  %%% "spire"               % "0.18.0",
      "org.typelevel"  %%% "spire-laws"          % "0.18.0"    % Test,
      "org.typelevel"  %%% "munit-cats-effect"   % "2.0.0-M4"  % Test,
      "org.typelevel"  %%% "discipline-munit"    % "2.0.0-M3"  % Test,
      "org.scalameta"  %%% "munit-scalacheck"    % "1.0.0-M11" % Test
    ),
    nativeLinkingOptions ++= List("-static").filter(_ => inDocker),
    nativeCompileOptions ++= List("-static").filter(_ => inDocker),
    scalacOptions ++= Seq(
      "-deprecation",
      "-source:future"
    )
  )

val stageBinary = taskKey[Unit]("Copy the binary to the top level")

stageBinary := {
  IO.copyFile((Compile / nativeLink).value, file("bot"))
}
