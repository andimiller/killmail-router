package space.inyour.horses.killmail.router
package formatters

import io.circe.{Codec, Json}
import template.Template

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

  def templateNamed(name: String, template: Template): Formatter = (j: Json) =>
    WebhookPayload(
      template.render(j),
      Some(name)
    )

}
