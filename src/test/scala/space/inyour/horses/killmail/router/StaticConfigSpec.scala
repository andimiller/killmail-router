package space.inyour.horses.killmail.router

import io.circe.syntax.*
import cats.effect.IO
import fs2.io.file.{Files, Path}
import io.circe.Json
import munit.CatsEffectSuite
import io.circe.yaml.syntax.*

class StaticConfigSpec extends CatsEffectSuite {

  test("should be able to load an example config and reformat it") {
    for
      input    <- Main.loadConfig[IO](Path("./src/test/resources/longconfig.yml"))
      json      = input.pretty.asJson
      yaml      = json.asYaml.spaces2
      expected <- Files[IO]
                    .readAll(Path("./src/test/resources/rewrittenconfig.yml"))
                    .through(fs2.text.utf8.decode)
                    .compile
                    .string
                    .map(_.replace("\r\n", "\n"))
    yield assertEquals(yaml, expected)
  }

  test("should also parse the pretty version") {
    Main.loadConfig[IO](Path("./src/test/resources/rewrittenconfig.yml"))
  }

}
