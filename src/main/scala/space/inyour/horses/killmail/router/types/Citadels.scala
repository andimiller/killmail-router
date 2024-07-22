package space.inyour.horses.killmail.router.types

import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import fs2.io.file.{Files, Path}
import io.circe.syntax.*
import io.circe.{Codec, Json, JsonNumber, JsonObject}
import space.inyour.horses.killmail.router.enrichers.Enricher

object Citadels {
  case class Citadel(typeName: String, typeID: Int) derives Codec.AsObject

  def load[F[_]: Files: Sync](p: Path): F[Vector[Citadel]] = {
    val stream = for {
      line <- Files[F].readAll(p).through(fs2.text.utf8.decode).through(fs2.text.lines)
      json <- fs2.Stream.eval(Sync[F].fromEither(io.circe.jawn.parse(line)))
      edge <- fs2.Stream.eval(Sync[F].fromEither(json.as[Citadel]))
    } yield edge
    stream.compile.toVector
  }

  def citadelEnricher(nodes: Vector[Citadel]): Enricher = {
    val citadels: Set[Int] = nodes.map(_.typeID).toSet

    (j: Json) => {
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
          val isCitadel     = value("ship_type_id").flatMap(_.as[Int].toOption).filter(citadels.contains).as(true).getOrElse(false)
          if (hasShipTypeId)
            value.add("is_citadel", Json.fromBoolean(isCitadel)).mapValues(_.foldWith(folder)).toJson
          else
            value.mapValues(_.foldWith(folder)).toJson
        }
        def onString(value: String): Json      =
          Json.fromString(value)
      }

      j.foldWith(folder)
    }
  }
}
