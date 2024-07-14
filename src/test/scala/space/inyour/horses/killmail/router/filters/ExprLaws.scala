package space.inyour.horses.killmail.router.filters

import io.circe.syntax.*
import algebra.Eq
import algebra.laws.LogicLaws
import io.circe.Json
import munit.DisciplineSuite

class ExprLaws extends DisciplineSuite with ExprInstances {
  {
    given Eq[Expr] = Eq.by { expr => run(expr)(Json.obj()).value }
    checkAll("{}", LogicLaws[Expr].bool)
  }
  {
    given Eq[Expr] = Eq.by { expr =>
      run(expr)(
        Json.obj(
          "a" := "a",
          "b" := "b",
          "c" := 5,
          "d" := Json.obj(
            "a" := 4
          )
        )
      ).value
    }
    checkAll("basic", LogicLaws[Expr].bool)
  }
}
