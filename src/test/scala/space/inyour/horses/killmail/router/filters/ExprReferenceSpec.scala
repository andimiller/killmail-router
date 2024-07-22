package space.inyour.horses.killmail.router.filters

import cats.implicits.*
import space.inyour.horses.killmail.router.filters.PathOperation.DownField
import io.circe.{Json, JsonNumber}

import scala.collection.immutable.ListMap
import scala.util.Try

class ExprReferenceSpec extends munit.FunSuite {

  test("Should be able to embed refs") {
    val result = Expr
      .resolveReferences(
        Expr.Reference("true"),
        ListMap(
          "true" -> Expr.Pure(true)
        )
      )
      .value
    assertEquals(
      result,
      Expr.Pure(true)
    )
  }

  test("Should throw on a bad ref") {
    val run = Try {
      Expr
        .resolveReferences(
          Expr.Reference("oops"),
          ListMap.empty
        )
        .value
    }
    assertEquals(
      run.failed.toOption.map(_.getMessage),
      Some("Unable to find binding for oops")
    )
  }

  test("Should resolve lets in order top down") {
    val Right(expr) = Expr.codec.parser.parseAll(
      """(let
        |  [
        |    (is-wormhole (< root.wormhole_class 7))
        |    (capital (== root.is_capital true))
        |    (faction (== root.tech_type 4))
        |    (faction-capital (and capital faction))
        |  ]
        |  (and
        |    (apply root.killmail is-wormhole)
        |    (or
        |      (apply root.killmail.victim faction-capital)
        |      (exists root.killmail.attackers faction-capital)
        |    )
        |  )
        |)""".stripMargin
    ): @unchecked

    val resolved = Expr.resolveReferences(expr).value

    assertEquals(
      resolved,
      Expr.And(
        Expr.Apply(List(DownField("killmail")), Expr.LessThan(List(DownField("wormhole_class")), JsonNumber.fromDecimalStringUnsafe("7"))),
        Expr.Or(
          Expr.Apply(
            List(DownField("killmail"), DownField("victim")),
            Expr.And(
              Expr.Equals(List(DownField("is_capital")), Json.fromBoolean(true)),
              Expr.Equals(List(DownField("tech_type")), Json.fromInt(4))
            )
          ),
          Expr.Exists(
            List(DownField("killmail"), DownField("attackers")),
            Expr.And(
              Expr.Equals(List(DownField("is_capital")), Json.fromBoolean(true)),
              Expr.Equals(List(DownField("tech_type")), Json.fromInt(4))
            )
          )
        )
      )
    )
  }
}
