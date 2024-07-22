package space.inyour.horses.killmail.router.filters

import cats.implicits.*
import cats.effect.IO
import cats.effect.kernel.Ref
import fs2.io.file.{Files, Path}
import io.circe.Json
import io.circe.jawn.JawnParser
import munit.CatsEffectSuite
import org.http4s.HttpRoutes
import org.http4s.client.Client
import org.http4s.dsl.io.*
import org.http4s.circe.*
import org.http4s.implicits.*
import space.inyour.horses.killmail.router.RulesEngine
import space.inyour.horses.killmail.router.redisq.RedisQ

class FilterIntegrationSpec extends CatsEffectSuite {

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

  test("Should load an item from RedisQ") {
    fakeRedisQ(Path("./src/test/resources/payload1.json")).stream.compile.count.assertEquals(1)
  }

  test("Should be able to run realistic filters against real data") {
    for
      results <- Ref.of[IO, Vector[Json]](Vector.empty)
      redisq   = fakeRedisQ(Path("./src/test/resources/payload1.json"))
      expr    <- Expr.parse[IO]("(contains root.killmail.attackers|{root.corporation_id} 98469412)")
      engine   = RulesEngine.withFilters(
                   expr -> { (j: Json) => results.update(_.appended(j)) }
                 )
      _       <- redisq.stream.through(engine).compile.drain
      _       <- results.get.map(_.size).assertEquals(1)
    yield ()
  }

}
