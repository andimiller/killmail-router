package space.inyour.horses.killmail.router

import io.circe.Codec
import org.http4s.Uri
import space.inyour.horses.killmail.router.filters.Expr
import utils.JsonCodecs.given

case class Route(
    name: String,
    filter: Expr,
    webhook: Uri
) derives Codec.AsObject

case class StaticConfig(
    routes: List[Route]
) derives Codec.AsObject
