package space.inyour.horses.killmail.router.types

import cats.effect.*
import fs2.io.file.{Files, Path}
import io.circe.Json
import io.circe.jawn.JawnParser
import io.circe.syntax.*
import munit.CatsEffectSuite
import org.http4s.HttpRoutes
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import space.inyour.horses.killmail.router.redisq.RedisQ

class CapitalsSpec extends CatsEffectSuite {

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

  test("should be able to enrich a killmail with capital data") {
    for {
      capitals <- Capitals.load[IO](Path("./capitals.json"))
      enricher  = Capitals.capitalShipEnricher(capitals)
      output   <- fakeRedisQ(Path("./src/test/resources/payload1.json")).stream
                    .map(enricher)
                    .compile
                    .toVector
      _         = assertEquals(
                    output.head.hcursor.downField("killmail").downField("victim").downField("is_capital").focus,
                    Some(Json.fromBoolean(false))
                  )
      _         = assertEquals(
                    output.head.hcursor.downField("killmail").downField("attackers").downN(0).downField("is_capital").focus,
                    Some(Json.fromBoolean(true))
                  )
    } yield ()
  }

}
