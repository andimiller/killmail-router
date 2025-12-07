package space.inyour.horses.killmail.router.esi

import cats.implicits.*
import cats.effect.Async
import io.circe.syntax.KeyOps
import io.circe.{Decoder, Json}
import org.http4s.circe.*
import org.http4s.Uri
import org.http4s.client.Client
import org.typelevel.log4cats.{LoggerFactory, SelfAwareStructuredLogger}
import space.inyour.horses.killmail.router.enrichers.EnricherF
import space.inyour.horses.killmail.router.schema.Schema

class EsiHydrator[F[_]: Async: LoggerFactory](client: Client[F]) extends EnricherF[F] {

  val logger: SelfAwareStructuredLogger[F] = LoggerFactory[F].getLogger

  given Decoder[Uri] = Decoder[String].emap(Uri.fromString(_).leftMap(_.toString))

  override def schema: Schema = Schema.zkillPayload

  override def apply(j: Json): F[Json] = {
    for
      href  <- Async[F].fromEither(j.hcursor.downField("zkb").downField("href").as[Uri])
      esiKm <- client.expect[Json](href)
    yield j.deepMerge(Json.obj("killmail" := esiKm))
  }.recoverWith { case t =>
    logger
      .error(t)(s"failed to hydrate km from esi given ${j.noSpaces}")
      .as(j.deepMerge(Json.obj("killmail" := Json.obj())))
  }

}
