package space.inyour.horses.killmail.router

import io.circe.*
import org.http4s.Uri
import space.inyour.horses.killmail.router.filters.{Expr, PrettyExpr}
import utils.JsonCodecs.given
import template.Template

case class PrettyRoute(
    name: String,
    filter: PrettyExpr,
    webhook: Uri,
    template: Template
) derives Codec.AsObject

case class Route(
    name: String,
    filter: Expr,
    webhook: Uri,
    template: Option[Template]
) derives Codec.AsObject {
  def pretty: PrettyRoute = PrettyRoute(name, filter.pretty, webhook, template.getOrElse(Template.default))
}

case class SiggyConfig(
    id: String,
    secret: String,
    chain: String,
    systems: List[Long]
) derives Codec.AsObject

case class StaticConfig(
    siggy: Option[SiggyConfig],
    routes: List[Route]
) derives Codec.AsObject {
  def pretty: PrettyStaticConfig = PrettyStaticConfig(siggy, routes.map(_.pretty))
}

case class PrettyStaticConfig(
    siggy: Option[SiggyConfig],
    routes: List[PrettyRoute]
) derives Codec.AsObject

object StaticConfig:
  lazy val permissiveFilterRewriter: Json.Folder[Json] = new Json.Folder[Json]:
    override def onBoolean(value: Boolean): Json    = Json.fromBoolean(value)
    override def onNull: Json                       = Json.Null
    override def onNumber(value: JsonNumber): Json  = Json.fromJsonNumber(value)
    override def onString(value: String): Json      = Json.fromString(value)
    override def onArray(value: Vector[Json]): Json = Json.arr(value.map(_.foldWith(permissiveFilterRewriter))*)
    override def onObject(value: JsonObject): Json  =
      value("filter") match
        case Some(expr) =>
          val rewritten = expr.asString.get.replace("\\n", "").strip()
          value
            .add("filter", Json.fromString(rewritten))
            .mapValues(_.foldWith(permissiveFilterRewriter))
            .toJson
        case None       => value.mapValues(_.foldWith(permissiveFilterRewriter)).toJson
