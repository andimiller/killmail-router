package space.inyour.horses.killmail.router.webhook

import cats.effect.kernel.Async
import org.http4s.{Method, Request, Uri}
import org.http4s.client.Client
import org.http4s.circe.*
import io.circe.syntax.*
import space.inyour.horses.killmail.router.formatters.WebhookPayload

trait DiscordWebhooks[F[_]] {
  def activate(uri: Uri, body: WebhookPayload): F[Unit]
}

object DiscordWebhooks {
  def create[F[_]: Async](client: Client[F]) = new DiscordWebhooks[F] {
    override def activate(uri: Uri, body: WebhookPayload): F[Unit] =
      client.expect(Request[F](Method.POST, uri.withQueryParam("wait", "true")).withEntity(body.asJson))
  }
}
