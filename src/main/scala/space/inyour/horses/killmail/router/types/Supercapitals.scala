package space.inyour.horses.killmail.router.types

import cats.effect.Sync
import cats.implicits.*
import fs2.io.file.{Files, Path}
import io.circe.{Codec, Json, JsonNumber, JsonObject}
import space.inyour.horses.killmail.router.enrichers.Enricher
import space.inyour.horses.killmail.router.schema.Schema
import space.inyour.horses.killmail.router.schema.Schema.*

object Supercapitals {
  case class Supercapital(typeName: String, typeID: Int) derives Codec.AsObject

  def load[F[_]: Files: Sync](p: Path): F[Vector[Supercapital]] = {
    val stream = for {
      line <- Files[F].readAll(p).through(fs2.text.utf8.decode).through(fs2.text.lines).filter(_.nonEmpty)
      json <- fs2.Stream.eval(Sync[F].fromEither(io.circe.jawn.parse(line)))
      edge <- fs2.Stream.eval(Sync[F].fromEither(json.as[Supercapital]))
    } yield edge
    stream.compile.toVector
  }

  def supercapitalShipEnricher(nodes: Vector[Supercapital]): Enricher = {
    val supercapitals: Set[Int] = nodes.map(_.typeID).toSet

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
            val hasShipTypeId   = value("ship_type_id").isDefined
            val isSupercapital  = value("ship_type_id").flatMap(_.as[Int].toOption).filter(supercapitals.contains).as(true).getOrElse(false)

            if (hasShipTypeId)
              value
                .add("is_supercapital", Json.fromBoolean(isSupercapital))
                .mapValues(_.foldWith(folder))
                .toJson
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
                      "is_supercapital" -> SBool
                    )
                  )
                ),
                "victim"    -> SObject(
                  Map(
                    "is_supercapital" -> SBool
                  )
                )
              )
            )
          )
        )
  }
}
