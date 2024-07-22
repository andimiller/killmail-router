package space.inyour.horses.killmail.router

import cats.implicits.*
import cats.effect.Async
import fs2.Pipe
import io.circe.Json
import org.typelevel.log4cats.LoggerFactory
import space.inyour.horses.killmail.router.filters.Expr
import space.inyour.horses.killmail.router.formatters.Formatter
import space.inyour.horses.killmail.router.webhook.DiscordWebhooks
import template.Template

object RulesEngine {
  def withFilters[F[_]: Async](rules: (Expr, Json => F[Unit])*): Pipe[F, Json, Unit] = { input =>
    input.evalMap { item =>
      rules.mapFilter { case (expr, run) => Option.when(expr.run(item))(run(item)) }.sequence_
    }
  }

  def fromStaticConfig[F[_]: Async: LoggerFactory](discordWebhooks: DiscordWebhooks[F], staticConfig: StaticConfig): Pipe[F, Json, Unit] = {
    val logger = LoggerFactory[F].getLoggerFromClass(classOf[RulesEngine.type])

    { input =>
      input.evalMap { item =>
        logger.info(item.noSpaces) *>
          staticConfig.routes.traverse_ { route =>
            if (route.filter.run(item)) {
              logger.info(s"matched ${route.name}, sending to webhook") *>
                discordWebhooks.activate(
                  route.webhook,
                  Formatter.templateNamed(route.name, route.template.getOrElse(Template.default))(item)
                )
            } else {
              logger.debug(s"didn't match ${route.name}")
            }
          }
      }
    }
  }
}
