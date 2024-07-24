package space.inyour.horses.killmail.router

import cats.implicits.*
import cats.*
import cats.data.{Ior, NonEmptyList, ValidatedNel}
import io.circe.Json
import io.circe.JsonNumber
import io.circe.JsonObject

package object schema {
  enum Schema:
    case SString
    case SInt
    case SDouble
    case SBool
    case SNull
    case SAny
    case SArray(values: Schema)
    case SObject(contents: Map[String, Schema])
    case SMap(values: Schema)
    case SAnyOf(values: NonEmptyList[Schema])

    def validate(j: Json): ValidatedNel[Schema.ValidationError, Json]        = Schema.validateInternal(this, j)
    def isContainedBy(s: Schema): Boolean                                    = (s |+| this) == s
    def validate(base: Schema): ValidatedNel[Schema.ValidationError, Schema] = Schema.validateSchemaInternal(base, this)
  import Schema.*

  object Schema:
    lazy implicit val show: Show[Schema] = Show.show {
      case SString           => "SString"
      case SInt              => "SInt"
      case SDouble           => "SDouble"
      case SBool             => "SBool"
      case SNull             => "SNull"
      case SAny              => "SAny"
      case SArray(values)    => show"SArray($values)"
      case SMap(values)      => show"SMap($values)"
      case SObject(contents) =>
        contents.toVector
          .sortBy(_._1)
          .map { case (name, schema) =>
            show"\"$name\" -> $schema"
          }
          .mkString("SObject(Map(", ", ", "))")
      case SAnyOf(values)    =>
        values.iterator.map(_.show).mkString("SAnyOf(NonEmptyList.of(", ", ", "))")
    }

    case class ValidationError(path: List[String], message: String) {
      def prependPath(s: String) = copy(path = s :: path)
    }
    object ValidationError                                          {
      given Show[ValidationError] = Show.show { case (ValidationError(path, message)) =>
        s"root${path.map(s => if (s.startsWith("[")) s else "." + s).mkString("")}: $message"
      }
    }

    private def validateSchemaInternal(expected: Schema, actual: Schema): ValidatedNel[ValidationError, Schema] = {
      (expected, actual) match {
        case (SAny, _)                                    => actual.validNel
        case (_, SAny)                                    => actual.validNel
        case (e, a) if e == a                             => actual.validNel
        case (SAnyOf(any), e) if any.iterator.contains(e) => actual.validNel
        case (SArray(e), SArray(a))                       => validateSchemaInternal(e, a).leftMap(_.map(_.prependPath("[*]")))
        case (SMap(values), SObject(a))                   =>
          a.toList
            .traverse { case (_, actualValueSpec) =>
              validateSchemaInternal(values, actualValueSpec)
            }
            .as(actual)
        case (SObject(e), SMap(values))                   =>
          e.toList
            .traverse { case (_, actualValueSpec) =>
              validateSchemaInternal(expected, values)
            }
            .as(actual)
        case (SObject(e), SObject(a))                     =>
          a.toList
            .traverse { case (name, actualValueSpec) =>
              e.get(name)
                .fold(
                  ValidationError(List.empty, show"Expected key called $name, it was not found").invalidNel[Schema]
                ) { expectedValueSpec =>
                  validateSchemaInternal(expectedValueSpec, actualValueSpec).leftMap(_.map(_.prependPath(name)))
                }
            }
            .as(actual)
        case (e, a)                                       => ValidationError(List.empty, show"Expected $e but found $a").invalidNel[Schema]
      }
    }

    private def validateInternal(expected: Schema, j: Json): ValidatedNel[ValidationError, Json] = {
      val actual = deriveSchemaForJson(j)
      expected match
        case e @ SArray(itemSpec)    =>
          j.asArray.fold(
            ValidationError(List.empty, show"Expected $e, found ${deriveSchemaForJson(j)}").invalidNel[Json]
          ) { values =>
            values.zipWithIndex
              .traverse { case (j, idx) =>
                validateInternal(itemSpec, j).leftMap(_.map(_.prependPath(show"[$idx]")))
              }
              .as(j)
          }
        case e @ SObject(contents)   =>
          j.asObject.fold(
            ValidationError(List.empty, show"Expected $e, found ${deriveSchemaForJson(j)}").invalidNel[Json]
          ) { obj =>
            obj.toList
              .traverse { case (name, value) =>
                contents
                  .get(name)
                  .fold(
                    value.validNel
                      // ValidationError(List.empty, show"Unexpected key: $name").invalidNel[Json]
                  ) { itemSpec =>
                    validateInternal(itemSpec, value)
                  }
                  .leftMap(_.map(_.prependPath(name)))
              }
              .as(j)
          }
        case e @ SMap(valueSpec)     =>
          j.asObject.fold(
            ValidationError(List.empty, show"Expected $e, found ${deriveSchemaForJson(j)}").invalidNel[Json]
          ) { obj =>
            obj.toList
              .traverse { case (name, value) =>
                validateInternal(valueSpec, value).leftMap(_.map(_.prependPath(name)))
              }
              .as(j)
          }
        case e @ SAnyOf(specs)       =>
          val results = specs.map(s => validateInternal(s, j))
          if (results.exists(_.isValid)) {
            j.validNel
          } else {
            results.sequence.as(j)
          }
        case SAny                    => j.validNel
        case e if expected != actual =>
          ValidationError(List.empty, show"Expected $e, found ${deriveSchemaForJson(j)}").invalidNel
        case _                       => j.validNel
    }

    given Semigroup[Schema] = new Semigroup[Schema] {
      override def combine(x: Schema, y: Schema): Schema = {
        (x, y) match {
          case (SArray(left), SArray(right))   => SArray(combine(left, right))
          case (SMap(left), SMap(right))       => SArray(combine(left, right))
          case (SObject(left), SObject(right)) =>
            SObject(
              left
                .align(right)
                .view
                .mapValues {
                  case Ior.Left(a)    => a
                  case Ior.Right(b)   => b
                  case Ior.Both(a, b) => combine(a, b)
                }
                .toMap
            )
          case (l, r) if l == r                => l
          case (l, r)                          => throw new RuntimeException(s"tried to combine unrelated schema types: $l, $r")
        }
      }
    }

    lazy val schemaFolder: Json.Folder[Schema] = new Json.Folder[Schema] {
      import Schema.*

      override def onString(value: String): Schema     = SString
      override def onNumber(value: JsonNumber): Schema = value.toInt.as(SInt).getOrElse(SDouble)
      override def onBoolean(value: Boolean): Schema   = SBool
      override def onNull: Schema                      = SNull

      override def onArray(value: Vector[Json]): Schema =
        SArray(
          value.map(_.foldWith(schemaFolder)).reduceOption(Semigroup[Schema].combine).getOrElse(SNull)
        )
      override def onObject(value: JsonObject): Schema  =
        SObject(
          value.toMap.view.mapValues(_.foldWith(schemaFolder)).toMap
        )
    }

    def deriveSchemaForJson(j: Json): Schema = j.foldWith(schemaFolder)

    val zkillPayload: Schema = {
      SObject(
        Map(
          "killID"   -> SInt,
          "killmail" -> SObject(
            Map(
              "killmail_time"   -> SString,
              "attackers"       -> SArray(
                SObject(
                  Map(
                    "final_blow"      -> SBool,
                    "character_id"    -> SInt,
                    "ship_type_id"    -> SInt,
                    "alliance_id"     -> SInt,
                    "damage_done"     -> SInt,
                    "security_status" -> SInt,
                    "weapon_type_id"  -> SInt,
                    "corporation_id"  -> SInt
                  )
                )
              ),
              "solar_system_id" -> SInt,
              "killmail_id"     -> SInt,
              "victim"          -> SObject(
                Map(
                  "items"          -> SArray(
                    SObject(
                      Map(
                        "item_type_id"       -> SInt,
                        "quantity_destroyed" -> SInt,
                        "flag"               -> SInt,
                        "singleton"          -> SInt,
                        "quantity_dropped"   -> SInt
                      )
                    )
                  ),
                  "character_id"   -> SInt,
                  "ship_type_id"   -> SInt,
                  "damage_taken"   -> SInt,
                  "alliance_id"    -> SInt,
                  "position"       -> SObject(Map("x" -> SDouble, "y" -> SDouble, "z" -> SDouble)),
                  "corporation_id" -> SInt
                )
              )
            )
          ),
          "zkb"      -> SObject(
            Map(
              "awox"           -> SBool,
              "npc"            -> SBool,
              "labels"         -> SArray(SString),
              "href"           -> SString,
              "fittedValue"    -> SDouble,
              "droppedValue"   -> SDouble,
              "hash"           -> SString,
              "points"         -> SInt,
              "totalValue"     -> SDouble,
              "locationID"     -> SInt,
              "destroyedValue" -> SDouble,
              "solo"           -> SBool
            )
          )
        )
      )
    }

}
