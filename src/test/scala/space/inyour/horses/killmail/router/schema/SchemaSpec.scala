package space.inyour.horses.killmail.router.schema

import cats.implicits.*
import Schema.*
import io.circe.*
import cats.effect.*
import cats.data.NonEmptyList
import fs2.io.file.{Files, Path}
import io.circe.jawn.JawnParser
import munit.CatsEffectSuite
import space.inyour.horses.killmail.router.filters.Expr
import space.inyour.horses.killmail.router.Main
import space.inyour.horses.killmail.router.types.{Capitals, Citadels, RigSize}
import space.inyour.horses.killmail.router.siggy.{ChainMap, Siggy, SiggyEnricher}
import space.inyour.horses.killmail.router.enrichers.Trumpets
import space.inyour.horses.killmail.router.maps.Systems

import scala.concurrent.duration.*

class SchemaSpec extends CatsEffectSuite {

  def loadFile(p: Path): IO[Json] = Files[IO].readAll(p).through(fs2.text.utf8.decode).compile.string.flatMap { s =>
    IO.fromEither(
      JawnParser(false).parse(s)
    )
  }

  test("Should be able to derive a schema from some JSON") {
    val schema = deriveSchemaForJson(
      Json.obj("message" -> Json.fromString("hello world"), "number" -> Json.fromInt(123), "float" -> Json.fromDouble(1.23).get)
    )

    assertEquals(
      schema,
      SObject(Map("message" -> SString, "number" -> SInt, "float" -> SDouble))
    )
  }

  test("Should be able to derive the main spec from an example payload") {
    loadFile(Path("./src/test/resources/payload1.json"))
      .map { j =>
        deriveSchemaForJson(j.hcursor.downField("package").focus.get)
      }
      .assertEquals(
        zkillPayload
      )

  }

  test("Should be able to verify an example payload") {
    loadFile(Path("./src/test/resources/payload2.json"))
      .map { j =>
        zkillPayload.validate(j.hcursor.downField("package").focus.get).as(())
      }
      .assertEquals(
        ().validNel
      )
  }

  test("Empty object should be valid") {
    assertEquals(
      zkillPayload.validate(Json.obj()).as(()),
      ().validNel
    )
  }

  test("Should be able to merge two specs") {
    val a = SObject(Map("a" -> SInt))
    val b = SObject(Map("b" -> SString))
    assertEquals(
      a |+| b,
      SObject(Map("a" -> SInt, "b" -> SString))
    )
  }

  test("Throw an error if keys collide when merging") {
    val a1 = SObject(Map("a" -> SInt))
    val a2 = SObject(Map("a" -> SString))
    intercept[RuntimeException] {
      a1 |+| a2
    }
  }

  test("Example config should only have one valid route against base schema") {
    for {
      input <- Main.loadConfig[IO](Path("./src/test/resources/longconfig.yml"))
    } yield assertEquals(
      input.routes.map(_.filter.schema.isContainedBy(zkillPayload)),
      List(true, false, false, false, false)
    )
  }

  test("Should be able to validate a more specific schema against a more general one") {

    val specific = SObject(Map("a" -> SInt))
    val general  = SObject(Map("a" -> SAnyOf(NonEmptyList.of(SInt, SBool))))

    assertEquals(
      specific.validate(general),
      specific.validNel
    )
  }

  test("Should be able to validate deep into a schema") {

    val specific = SObject(Map("a" -> SArray(SObject(Map("b" -> SInt)))))
    val general  = SObject(Map("a" -> SArray(SObject(Map("b" -> SString)))))

    assertEquals(
      specific.validate(general),
      ValidationError(List("a", "[*]", "b"), "Expected SString but found SInt").invalidNel
    )
  }

  test("Example config should all be valid except one, when run against combined schemas") {
    for {
      input   <- Main.loadConfig[IO](Path("./src/test/resources/longconfig.yml"))
      capitals = Capitals.capitalShipEnricher(Vector.empty)
      citadels = Citadels.citadelEnricher(Vector.empty)
      rigsize  = RigSize.rigSizeEnricher(Vector.empty)
      systems  = Systems.wormholeClassEnricher(Vector.empty)
      siggy   <- SiggyEnricher.forSystem[IO](new Siggy[IO] {
                   override def getChainmap(id: String): IO[ChainMap] = IO {
                     ChainMap(1234, "default", List.empty)
                   }
                   override def listChainmaps: IO[Json]               = IO { Json.obj() }
                 })("chain", 1234L, 1.minute)
      trumpet  = Trumpets
      combined = NonEmptyList
                   .of(
                     capitals.liftF[IO],
                     citadels.liftF[IO],
                     rigsize.liftF[IO],
                     siggy,
                     trumpet.liftF[IO],
                     systems.liftF[IO]
                   )
                   .reduce
                   .schema |+| zkillPayload
    } yield assertEquals(
      input.routes.map(_.filter.schema.validate(combined)),
      List(
        SObject(
          Map(
            "killmail" -> SObject(
              Map("victim" -> SObject(Map("alliance_id" -> SInt)), "attackers" -> SArray(SObject(Map("alliance_id" -> SInt))))
            )
          )
        ).validNel,
        SObject(
          Map(
            "killmail" -> SObject(
              Map(
                "victim"         -> SObject(Map("is_capital" -> SBool)),
                "attackers"      -> SArray(SObject(Map("is_capital" -> SBool))),
                "wormhole_class" -> SInt
              )
            )
          )
        ).validNel,
        SObject(
          Map(
            "killmail" -> SObject(
              Map(
                "victim"         -> SObject(Map("is_capital" -> SBool)),
                "attackers"      -> SArray(SObject(Map("is_capital" -> SBool))),
                "wormhole_class" -> SInt
              )
            )
          )
        ).validNel,
        SObject(
          Map(
            "killmail" -> SObject(
              Map(
                "victim"         -> SObject(Map("is_capital" -> SBool)),
                "attackers"      -> SArray(SObject(Map("is_capital" -> SBool))),
                "wormhole_class" -> SInt
              )
            )
          )
        ).validNel,
        NonEmptyList.of(ValidationError(List(), "Expected key called items, it was not found")).invalid
      )
    )
  }

}
