package space.inyour.horses.killmail.router.enrichers

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

class TrumpetsSpec extends CatsEffectSuite {

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

  test("should add an empty string on low value kills") {
    for {
      output <- fakeRedisQ(Path("./src/test/resources/payload1.json")).stream
                  .map(Trumpets)
                  .compile
                  .toVector
      _       = assertEquals(
                  output.head.hcursor.downField("trumpets").focus,
                  Some(Json.fromString(""))
                )
    } yield ()
  }

  test("should add a few trumpets on a higher value kill") {
    for {
      output <- fakeRedisQ(Path("./src/test/resources/payload2.json")).stream
                  .map(Trumpets)
                  .compile
                  .toVector
      _       = assertEquals(
                  output.head.hcursor.downField("trumpets").focus,
                  Some(Json.fromString(Trumpets.trumpet * 2))
                )
    } yield ()
  }

}
