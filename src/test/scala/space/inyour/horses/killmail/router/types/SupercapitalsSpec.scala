package space.inyour.horses.killmail.router.types

import cats.implicits.*
import cats.effect.*
import fs2.io.file.{Files, Path}
import io.circe.Json
import io.circe.jawn.JawnParser
import munit.CatsEffectSuite
import org.http4s.HttpRoutes
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import space.inyour.horses.killmail.router.redisq.RedisQ

class SupercapitalsSpec extends CatsEffectSuite {

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

  test("should be able to enrich a killmail with supercapital data") {
    // Use attacker's ship_type_id (19722) as test supercapital to verify enricher works
    val testSupercapitals = Vector(Supercapitals.Supercapital("Test Supercapital", 19722))
    val enricher          = Supercapitals.supercapitalShipEnricher(testSupercapitals)

    for {
      output <- fakeRedisQ(Path("./src/test/resources/payload1.json")).stream
                  .map(enricher)
                  .compile
                  .toVector
      // Victim (ship_type_id 12038) is not in supercapitals list
      _       = assertEquals(
                  output.head.hcursor.downField("killmail").downField("victim").downField("is_supercapital").focus,
                  Some(Json.fromBoolean(false))
                )
      // Attacker (ship_type_id 19722) is in our test supercapitals list
      _       = assertEquals(
                  output.head.hcursor.downField("killmail").downField("attackers").downN(0).downField("is_supercapital").focus,
                  Some(Json.fromBoolean(true))
                )
      // Schema validation passes
      _       = assertEquals(
                  enricher.schema.validate(output.head).as(()),
                  ().validNel
                )
    } yield ()
  }

  test("should mark all ships as non-supercapital when list is empty") {
    val enricher = Supercapitals.supercapitalShipEnricher(Vector.empty)

    for {
      output <- fakeRedisQ(Path("./src/test/resources/payload1.json")).stream
                  .map(enricher)
                  .compile
                  .toVector
      _       = assertEquals(
                  output.head.hcursor.downField("killmail").downField("victim").downField("is_supercapital").focus,
                  Some(Json.fromBoolean(false))
                )
      _       = assertEquals(
                  output.head.hcursor.downField("killmail").downField("attackers").downN(0).downField("is_supercapital").focus,
                  Some(Json.fromBoolean(false))
                )
    } yield ()
  }

  test("should be able to load supercapitals.json") {
    for {
      supercapitals <- Supercapitals.load[IO](Path("./supercapitals.json"))
      _              = assert(supercapitals.nonEmpty, "supercapitals.json should not be empty")
      _              = assert(supercapitals.exists(_.typeName == "Erebus"), "should contain Erebus titan")
      _              = assert(supercapitals.exists(_.typeName == "Nyx"), "should contain Nyx supercarrier")
    } yield ()
  }

}
