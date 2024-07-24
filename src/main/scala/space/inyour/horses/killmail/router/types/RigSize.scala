package space.inyour.horses.killmail.router.types

import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import cats.data.NonEmptyList
import fs2.io.file.{Files, Path}
import io.circe.syntax.*
import io.circe.{Codec, Json, JsonNumber, JsonObject}
import space.inyour.horses.killmail.router.enrichers.Enricher
import space.inyour.horses.killmail.router.schema.Schema
import space.inyour.horses.killmail.router.schema.Schema.*

object RigSize {
  case class RigSize(typeName: String, typeID: Int, size: Double) derives Codec.AsObject

  def load[F[_]: Files: Sync](p: Path): F[Vector[RigSize]] = {
    val stream = for {
      line <- Files[F].readAll(p).through(fs2.text.utf8.decode).through(fs2.text.lines)
      json <- fs2.Stream.eval(Sync[F].fromEither(io.circe.jawn.parse(line)))
      edge <- fs2.Stream.eval(Sync[F].fromEither(json.as[RigSize]))
    } yield edge
    stream.compile.toVector
  }

  def rigSizeEnricher(nodes: Vector[RigSize]): Enricher = {
    val lookup: Map[Int, Double] =
      nodes.groupMapReduce(_.typeID)(_.size)((_, _) => throw new Exception("Duplicate type ID in rig db")) // merge should never happen

    new Enricher:
      override def apply(j: Json): Json = {
        lazy val folder: Json.Folder[Json] = new Json.Folder[Json] {
          def onArray(value: Vector[Json]): Json =
            Json.arr(value.map(_.foldWith(folder))*)
          def onBoolean(value: Boolean): Json    =
            Json.fromBoolean(value)
          def onNull: Json                       = Json.Null
          def onNumber(value: JsonNumber): Json  =
            Json.fromJsonNumber(value)
          def onObject(value: JsonObject): Json  = {
            val hasShipTypeId = value("ship_type_id").isDefined
            val rigSize       = value("ship_type_id").flatMap(_.as[Int].toOption).flatMap(lookup.get).map {
              case 0.0 => Json.Null
              case 1.0 => Json.fromString("S")
              case 2.0 => Json.fromString("M")
              case 3.0 => Json.fromString("L")
              case 4.0 => Json.fromString("XL")
            }
            if (hasShipTypeId)
              value.add("rig_size", rigSize.getOrElse(Json.Null)).mapValues(_.foldWith(folder)).toJson
            else
              value.mapValues(_.foldWith(folder)).toJson
          }
          def onString(value: String): Json      =
            Json.fromString(value)
        }

        j.foldWith(folder)
      }

      override def schema: Schema =
        SObject(
          Map(
            "killmail" -> SObject(
              Map(
                "attackers" -> SArray(
                  SObject(
                    Map(
                      "rig_size" -> SAnyOf(NonEmptyList.of(SString, SNull))
                    )
                  )
                ),
                "victim"    -> SObject(
                  Map(
                    "rig_size" -> SAnyOf(NonEmptyList.of(SString, SNull))
                  )
                )
              )
            )
          )
        )

  }
}
