package space.inyour.horses.killmail.router.maps

import cats.effect.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.client.Client
import fs2.io.file.Path
import munit.CatsEffectSuite
import space.inyour.horses.killmail.router.redisq.RedisQ
import fs2.io.file.Files
import io.circe.jawn.JawnParser
import org.http4s.HttpRoutes
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.circe.*
import org.http4s.implicits.*

class SystemsSpec extends CatsEffectSuite {

  def loadFile(p: Path): IO[Json] = Files[IO].readAll(p).through(fs2.text.utf8.decode).compile.string.flatMap { s =>
    IO.fromEither(
      JawnParser(false).parse(s)
    )
  }

  def fakeRedisQ(p: Path): RedisQ[IO] = RedisQ.create[IO](
    Client.fromHttpApp(
      HttpRoutes
        .of[IO] { _ =>
          loadFile(p).flatMap(Ok(_))
        }
        .orNotFound
    ),
    "test"
  )

  test("should be able to enrich a killmail with wormhole data") {
    for {
      systems   <- Systems.load[IO](Path("./systems.json"))
      whEnricher = Systems.wormholeClassEnricher(systems)
      output    <- fakeRedisQ(Path("./src/test/resources/payload1.json")).stream
                     .map(whEnricher)
                     .map(_.hcursor.downField("killmail").focus.get.withObject(_.filterKeys(_ == "wormhole_class").toJson))
                     .compile
                     .toVector
      _          = assertEquals(output, Vector(Json.obj("wormhole_class" := 9)))
    } yield ()
  }

}
