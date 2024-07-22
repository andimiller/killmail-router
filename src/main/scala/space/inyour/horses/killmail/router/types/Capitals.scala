package space.inyour.horses.killmail.router.types

import cats.Monad
import cats.effect.Sync
import cats.implicits.*
import fs2.io.file.{Files, Path}
import io.circe.syntax.*
import io.circe.{Codec, Json, JsonNumber, JsonObject}
import space.inyour.horses.killmail.router.enrichers.Enricher

object Capitals {
  case class Capital(typeName: String, typeID: Int, techType: Int) derives Codec.AsObject

  def load[F[_]: Files: Sync](p: Path): F[Vector[Capital]] = {
    val stream = for {
      line <- Files[F].readAll(p).through(fs2.text.utf8.decode).through(fs2.text.lines).filter(_.nonEmpty)
      json <- fs2.Stream.eval(Sync[F].fromEither(io.circe.jawn.parse(line)))
      edge <- fs2.Stream.eval(Sync[F].fromEither(json.as[Capital]))
    } yield edge
    stream.compile.toVector
  }

  def capitalShipEnricher(nodes: Vector[Capital]): Enricher = {
    val capitals: Map[Int, Capital] = nodes.groupBy(_.typeID).view.mapValues(_.head).toMap

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
          val isCapital     = value("ship_type_id").flatMap(_.as[Int].toOption).filter(capitals.contains).as(true).getOrElse(false)
          val techType      = value("ship_type_id").flatMap(_.as[Int].toOption).flatMap(capitals.get).map(_.techType)

          if (hasShipTypeId)
            value.add("is_capital", Json.fromBoolean(isCapital)).add("tech_type", techType.fold(Json.Null)(Json.fromInt)).mapValues(_.foldWith(folder)).toJson
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
