package space.inyour.horses.killmail.router.siggy

import cats.implicits.*
import cats.effect.*
import fs2.io.file.{Files, Path}
import io.circe.Json
import io.circe.jawn.JawnParser
import io.circe.syntax.*
import munit.{CatsEffectSuite, ScalaCheckSuite}
import org.http4s.HttpRoutes
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.circe.CirceEntityCodec.*
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import net.andimiller.hedgehogs.*
import scala.concurrent.duration.*

class SiggySpec extends CatsEffectSuite {
  def loadFile(p: Path): IO[Json] = Files[IO].readAll(p).through(fs2.text.utf8.decode).compile.string.flatMap { s =>
    IO.fromEither(
      JawnParser(false).parse(s)
    )
  }

  test("Siggy client should load a real payload as a graph") {

    val fakeClient = Client.fromHttpApp(
      HttpRoutes
        .of[IO] { case GET -> Root / "api" / "v1" / "chainmaps" / "1" =>
          loadFile(Path("./src/test/resources/siggychain.json")).flatMap(Ok(_))
        }
        .orNotFound
    )
    for graph <- Siggy.create[IO](fakeClient, "id", "secret").getChainmap("1").map(_.toGraph)
    yield assertEquals(
      graph,
      Graph(
        Map(31002238L -> (), 30000142L -> (), 31000872L -> (), 31002233L -> ()),
        Map(
          31002238L   -> Vector((31000872L, 1), (30000142L, 1), (31002233L, 1)),
          30000142L   -> Vector((31002238L, 1)),
          31000872L   -> Vector((31002238L, 1)),
          31002233L   -> Vector((31002238L, 1))
        )
      )
    )

  }

  test("Siggy enricher should be able to enrich jump count") {

    val fakeClient = Client.fromHttpApp(
      HttpRoutes
        .of[IO] { case GET -> Root / "api" / "v1" / "chainmaps" / "1" =>
          loadFile(Path("./src/test/resources/siggychain.json")).flatMap(Ok(_))
        }
        .orNotFound
    )
    val siggy      = Siggy.create[IO](fakeClient, "id", "secret")
    for
      enricher1 <- SiggyEnricher.forSystem(siggy)("1", 31002233L, 1.minute)
      enricher2 <- SiggyEnricher.forSystem(siggy)("1", 1234L, 1.minute)
      combined   = enricher1 combine enricher2
      result    <- combined(Json.obj("nothing" := "useful"))
      _          = assertEquals(result, Json.obj("nothing" := "useful"))
      _          = assertEquals(
                     enricher1.schema.validate(result).as(()),
                     ().validNel
                   )
      result2   <- combined(Json.obj("killmail" := Json.obj("solar_system_id" := 30000142L)))
      _          = assertEquals(
                     result2,
                     Json.obj(
                       "chain_distance" := Json.obj("1234" := Json.Null, "31002233" := 2),
                       "killmail"       := Json.obj("solar_system_id" := 30000142L)
                     )
                   )
      _          = assertEquals(
                     enricher2.schema.validate(result2).as(()),
                     ().validNel
                   )
    yield ()

  }

}
