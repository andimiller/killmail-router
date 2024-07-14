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

}
