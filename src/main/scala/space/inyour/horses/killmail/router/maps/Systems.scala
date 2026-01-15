package space.inyour.horses.killmail.router.maps

import space.inyour.horses.killmail.router.enrichers.Enricher
import space.inyour.horses.killmail.router.schema.Schema
import io.circe.{Codec, Json, JsonNumber, JsonObject}
import net.andimiller.hedgehogs.Node
import net.andimiller.hedgehogs.circe.*
import cats.Monad
import cats.implicits.*
import cats.effect.Sync
import fs2.io.file.Files
import fs2.io.file.Path
import io.circe.syntax.*

object Systems {
  case class System(name: String, wormhole_class: Option[Int], region_id: Int, region_name: String) derives Codec.AsObject

  def load[F[_]: Files: Sync](p: Path): F[Vector[Node[Int, System]]] = {
    val stream = for {
      line <- Files[F].readAll(p).through(fs2.text.utf8.decode).through(fs2.text.lines).filter(_.nonEmpty)
      json <- fs2.Stream.eval(Sync[F].fromEither(io.circe.jawn.parse(line)))
      edge <- fs2.Stream.eval(Sync[F].fromEither(json.as[Node[Int, System]]))
    } yield edge
    stream.compile.toVector
  }

  def wormholeClassEnricher(nodes: Vector[Node[Int, System]]): Enricher = {
    val lookup: Map[Int, System] = nodes.map(n => n.id -> n.data).toMap

    new Enricher:
      override def apply(j: Json): Json = {
        lazy val folder: Json.Folder[Json] = new Json.Folder[Json] {
          def onArray(value: Vector[Json]): Json =
            Json.arr(value.map(_.foldWith(folder))*)

          def onBoolean(value: Boolean): Json =
            Json.fromBoolean(value)

          def onNull: Json = Json.Null

          def onNumber(value: JsonNumber): Json =
            Json.fromJsonNumber(value)

          def onObject(value: JsonObject): Json = {
            val hasSolarSystemID = value("solar_system_id").isDefined
            val system           = value("solar_system_id").flatMap(_.as[Int].toOption).flatMap(lookup.get)
            if (hasSolarSystemID)
              value
                .add("wormhole_class", system.flatMap(_.wormhole_class).map(Json.fromInt).getOrElse(Json.Null))
                .add("region_id", system.map(s => Json.fromInt(s.region_id)).getOrElse(Json.Null))
                .add("region_name", system.map(s => Json.fromString(s.region_name)).getOrElse(Json.Null))
                .mapValues(_.foldWith(folder))
                .toJson
            else
              value.mapValues(_.foldWith(folder)).toJson
          }

          def onString(value: String): Json =
            Json.fromString(value)
        }

        j.foldWith(folder)
      }

      override def schema: Schema = Schema.SObject(
        Map(
          "killmail" -> Schema.SObject(
            Map(
              "wormhole_class" -> Schema.SInt,
              "region_id"      -> Schema.SInt,
              "region_name"    -> Schema.SString
            )
          )
        )
      )

  }
}
