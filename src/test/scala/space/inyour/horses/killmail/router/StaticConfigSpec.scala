package space.inyour.horses.killmail.router

import io.circe.syntax.*
import cats.effect.IO
import fs2.io.file.Path
import munit.CatsEffectSuite
import io.circe.scalayaml.Printer

class StaticConfigSpec extends CatsEffectSuite {

  test("should be able to load an example config and reformat it") {
    Main
      .loadConfig[IO](Path("./src/test/resources/longconfig.yml"))
      .map(_.pretty.asJson)
      .map { j =>
        Printer(explicitStart = true).pretty(j)
      }
      .flatMap(IO.println)
  }

}
