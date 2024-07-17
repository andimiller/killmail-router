package space.inyour.horses.killmail.router.filters

import cats.implicits.*
import io.circe.{Json, JsonNumber}
import org.scalacheck.{Arbitrary, Gen}

trait ExprInstances {

  // syntax to make things easier to write
  private inline def gen[T](using a: Arbitrary[T]): Gen[T] = a.arbitrary

  given Arbitrary[Json] = Arbitrary(
    Gen.resize(
      5,
      Gen.recursive { recurse =>
        Gen.sized { depth =>
          inline def safeRecurse = Gen.resize(depth - 1, recurse)
          Gen.oneOf(
            gen[Boolean].map(Json.fromBoolean),
            Gen.alphaStr.map(Json.fromString),
            Gen.chooseNum(0, 10).map(Json.fromInt),
            Gen.listOf(safeRecurse).map(js => Json.arr(js*)),
            Gen
              .listOf(
                for
                  name  <- Gen.alphaStr
                  value <- safeRecurse
                yield name -> value
              )
              .map(kvs => Json.obj(kvs*))
          )
        }
      }
    )
  )

  given Arbitrary[PathOperation] = Arbitrary(
    Gen.resize(
      5,
      Gen.recursive { recurse =>
        Gen.sized { depth =>
          inline def safeRecurse = Gen.resize(depth - 1, recurse)
          Gen.oneOf(
            Gen.alphaStr.map(PathOperation.DownField.apply),
            Gen.chooseNum(0, 10).map(PathOperation.DownIndex.apply),
            pathGen.map(PathOperation.MapArrayPath.apply)
          )
        }
      }
    )
  )

  private inline def pathGen: Gen[List[PathOperation]] = Gen.listOf(gen[PathOperation])

  val veryBasicExpr: Arbitrary[Expr] = Arbitrary(
    Gen.oneOf(
      gen[Boolean].map(Expr.Pure.apply),
      for
        value <- gen[Json]
        path   = List.empty[PathOperation]
      yield Expr.Equals(path, value)
    )
  )

  val basicExpr: Arbitrary[Expr] = Arbitrary(
    Gen.oneOf[Expr](
      // basics
      gen[Boolean].map(Expr.Pure.apply),
      // comparisons
      for
        path  <- pathGen
        value <- gen[Json]
      yield Expr.Equals(path, value),
      for
        path  <- pathGen
        value <- Gen.chooseNum(0, 10)
      yield Expr.GreaterThan(path, JsonNumber.fromDecimalStringUnsafe(value.toString)),
      for
        path  <- pathGen
        value <- Gen.chooseNum(0, 10)
      yield Expr.LessThan(path, JsonNumber.fromDecimalStringUnsafe(value.toString)),
      for
        path  <- pathGen
        value <- Gen.oneOf(
                   Gen.alphaStr.map(Json.fromString),
                   Gen.chooseNum(0, 10).map(Json.fromInt)
                 )
      yield Expr.Contains(path, value)
    )
  )

  given fullExpr: Arbitrary[Expr] = Arbitrary(
    Gen.resize(
      5,
      Gen.recursive { recurse =>
        Gen.sized {
          case depth if depth > 0 =>
            inline def safeRecurse = Gen.resize(depth - 1, recurse)
            Gen.oneOf[Expr](
              basicExpr.arbitrary,
              safeRecurse.map(Expr.Not.apply),
              for
                l <- safeRecurse
                r <- safeRecurse
              yield Expr.And(l, r),
              for
                l <- safeRecurse
                r <- safeRecurse
              yield Expr.Or(l, r),
              for
                p <- pathGen
                e <- safeRecurse
              yield Expr.Exists(p, e)
            )
          case 0                  =>
            basicExpr.arbitrary
        }
      }
    )
  )
}
