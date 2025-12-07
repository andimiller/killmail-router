package space.inyour.horses.killmail.router.esi

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.KeyOps
import munit.CatsEffectSuite
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.*

class EsiHydratorSpec extends CatsEffectSuite {

  val fakeClient          = Client.fromHttpApp(
    HttpRoutes
      .of[IO] { case GET -> Root / "km" / "123" =>
        Ok(Json.obj("data" := "here"))
      }
      .orNotFound
  )
  given LoggerFactory[IO] = NoOpFactory[IO]

  val esiHydrator = new EsiHydrator[IO](fakeClient)

  test("Pull the url in the href and hydrate with it") {
    esiHydrator(
      Json.obj("zkb" := Json.obj("href" := "http://localhost/km/123"))
    )
      .assertEquals(
        Json.obj("zkb" := Json.obj("href" := "http://localhost/km/123"), "killmail" := Json.obj("data" := "here"))
      )
  }

  test("Fall back if there's no href") {
    esiHydrator(
      Json.obj("zkb" := Json.obj("other" := "data"))
    )
      .assertEquals(
        Json.obj("zkb" := Json.obj("other" := "data"), "killmail" := Json.obj())
      )
  }

  test("Fall back with bad href") {
    esiHydrator(
      Json.obj("zkb" := Json.obj("href" := "http://localhost/km/404"))
    )
      .assertEquals(
        Json.obj("zkb" := Json.obj("href" := "http://localhost/km/404"), "killmail" := Json.obj())
      )
  }

}
