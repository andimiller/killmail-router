package space.inyour.horses.killmail.router.formatters

import io.circe.{Codec, Json}

case class WebhookPayload(
    content: String,
    username: Option[String] = None
) derives Codec.AsObject

trait Formatter {
  def apply(j: Json): WebhookPayload
}

object Formatter {

  val simple: Formatter = (j: Json) =>
    WebhookPayload(
      s"https://zkillboard.com/kill/${j.hcursor.downField("killID").focus.flatMap(_.as[Int].toOption).getOrElse("unknown")}/",
      Some("killfeed")
    )

  def simpleNamed(name: String): Formatter = (j: Json) =>
    WebhookPayload(
      s"https://zkillboard.com/kill/${j.hcursor.downField("killID").focus.flatMap(_.as[Int].toOption).getOrElse("unknown")}/",
      Some(name)
    )

}
