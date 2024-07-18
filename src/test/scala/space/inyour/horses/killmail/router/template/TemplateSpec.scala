package space.inyour.horses.killmail.router.template

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
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import space.inyour.horses.killmail.router.filters.ExprInstances

class TemplateSpec extends CatsEffectSuite with ScalaCheckSuite with ExprInstances {
  def loadFile(p: Path): IO[Json] = Files[IO].readAll(p).through(fs2.text.utf8.decode).compile.string.flatMap { s =>
    IO.fromEither(
      JawnParser(false).parse(s)
    ).map(_.hcursor.downField("package").focus.get)
  }

  test("Default template should render a payload") {
    for
      json    <- loadFile(Path("./src/test/resources/payload1.json"))
      rendered = Template.default.render(json)
    yield assertEquals(rendered, "https://zkillboard.com/kill/119455394")
  }

  given Arbitrary[Template] = Arbitrary(
    Gen
      .nonEmptyListOf(
        Gen.oneOf(
          pathGen.map(TemplateExpr.Variable(_)),
          Gen.alphaStr.filter(_.nonEmpty).map(TemplateExpr.Literal(_))
        )
      )
      .map(exprs => Template(exprs.toVector))
  )

  property("Template show and parser should roundtrip, when simplified") {
    forAll { (t: Template) =>
      Template.parser.parseAll(t.show) == Right(t.simplify)
    }
  }

  test("Should do more advanced templates") {
    for
      json           <- loadFile(Path("./src/test/resources/payload1.json"))
      json2           = json.deepMerge(
                          Json.obj("chain_distance" := Json.obj("31002238" := 5))
                        )
      Right(template) = Template.parser.parseAll(
                          "Activity ${root.chain_distance.31002238} jumps down the Rage chain https://zkillboard.com/kill/${root.killID}"
                        ): @unchecked
      rendered        = template.render(json2)
    yield assertEquals(rendered, "Activity 5 jumps down the Rage chain https://zkillboard.com/kill/119455394")
  }

}
