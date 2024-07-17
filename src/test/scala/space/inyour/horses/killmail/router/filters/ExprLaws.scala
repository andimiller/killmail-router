package space.inyour.horses.killmail.router.filters

import algebra.Eq
import algebra.laws.LogicLaws
import io.circe.Json
import io.circe.syntax.*
import munit.{DisciplineSuite, ScalaCheckSuite}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

class ExprLaws extends DisciplineSuite with ExprInstances with ScalaCheckSuite {
  {
    given Eq[Expr] = Eq.by { expr => Expr.run(expr)(Json.obj()).value }
    checkAll("{}", LogicLaws[Expr].bool)
  }
  {
    given Eq[Expr] = Eq.by { expr =>
      Expr
        .run(expr)(
          Json.obj(
            "a" := "a",
            "b" := "b",
            "c" := 5,
            "d" := Json.obj(
              "a" := 4
            )
          )
        )
        .value
    }
    checkAll("basic", LogicLaws[Expr].bool)
  }
  import cats.implicits.*

  property("parser/printer roundtrip") {
    forAll { (expr: Expr) =>
      Expr.codec.parser.parseAll(
        Expr.codec.encode(expr)
      ) == Right(expr)
    }
  }

  property("printing any expression should never be multiline") {
    forAll { (expr: Expr) =>
      Expr.codec.encode(expr).linesIterator.size == 1
    }
  }

  property("parser/printer roundtrip with multiline") {
    forAll { (expr: Expr) =>
      Expr.codec.parser.parseAll(
        expr.pretty.show
      ) == Right(expr)
    }
  }

  test("Manual test") {
    val Right(expr) = Expr.codec.parser.parseAll("(== root 1234)"): @unchecked
    assertEquals(
      Expr.run(expr)(Json.fromInt(1234)).value,
      true
    )
    assertEquals(
      Expr.run(expr)(Json.fromInt(123)).value,
      false
    )
  }

  test("Manual test with bool literals") {
    val Right(expr) = Expr.codec.parser.parseAll("(== root true)"): @unchecked
    assertEquals(
      Expr.run(expr)(Json.fromBoolean(true)).value,
      true
    )
    assertEquals(
      Expr.run(expr)(Json.fromBoolean(false)).value,
      false
    )
  }

  test("Pretty printing") {
    val Right(expr) = Expr.codec.parser.parseAll(
      "(and (or (== root.killmail.victim.is_capital true) (contains root.killmail.attackers|{root.is_capital} true)) (== root.killmail.wormhole_class 6))"
    ): @unchecked
    assertEquals(
      expr.pretty.show,
      """(and
        |  (or
        |    (== root.killmail.victim.is_capital true)
        |    (contains root.killmail.attackers|{root.is_capital} true)
        |  )
        |  (== root.killmail.wormhole_class 6)
        |)""".replace("\r\n", "\n").stripMargin
    )
  }

  test("Map an expression onto a path") {
    val Right(expr) = Expr.codec.parser.parseAll(
      "(exists root.items (and (== root.a 1) (== root.b 2)))"
    ): @unchecked

    assertEquals(
      Expr
        .run(expr)(
          Json.obj(
            "items" := List(
              Json.obj("a" := 1),
              Json.obj("a" := 2),
              Json.obj("a" := 1, "b" := 2),
              Json.obj("b" := 2)
            )
          )
        )
        .value,
      true
    )

    assertEquals(
      Expr
        .run(expr)(
          Json.obj(
            "items" := List(
              Json.obj("a" := 1),
              Json.obj("a" := 2),
              Json.obj("b" := 2)
            )
          )
        )
        .value,
      false
    )

  }

}
