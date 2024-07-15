package space.inyour.horses.killmail.router

import cats.data.NonEmptyList
import cats.effect.*
import cats.implicits.*
import fs2.io.file.{Files, Path}
import fs2.io.net.Network
import fs2.io.net.tls.TLSContext
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.LoggerFactory
import space.inyour.horses.killmail.router.enrichers.Enricher
import space.inyour.horses.killmail.router.formatters.WebhookPayload
import space.inyour.horses.killmail.router.redisq.RedisQ
import space.inyour.horses.killmail.router.types.{Capitals, Citadels, RigSize}
import space.inyour.horses.killmail.router.maps.Systems
import space.inyour.horses.killmail.router.webhook.DiscordWebhooks

import scala.concurrent.duration.*

object Main extends IOApp {

  def resources[F[_]: Concurrent: Async: Network: LoggerFactory] =
    for
      tls    <- TLSContext.Builder.forAsync[F].insecureResource
      client <- EmberClientBuilder
                  .default[F]
                  .withTLSContext(tls)
                  .build
      retry   = Retry.create[F](RetryPolicy(RetryPolicy.exponentialBackoff(10.minutes, 10), RetryPolicy.defaultRetriable[F]))(client)
    yield retry

  def program[F[_]: Concurrent: Async: Network: Files: LoggerFactory](staticConfig: StaticConfig): F[Unit] =
    resources.use { client =>
      val webhooks = DiscordWebhooks.create(client)
      val redisq   = RedisQ.create(client, "andi-local-test")
      val engine   = RulesEngine.fromStaticConfig(webhooks, staticConfig)
      for
        enricher <-
          NonEmptyList
            .of(
              Systems.load[F](Path("./systems.json")).map(Systems.wormholeClassEnricher),
              Capitals.load[F](Path("./capitals.json")).map(Capitals.capitalShipEnricher),
              Citadels.load[F](Path("./citadels.json")).map(Citadels.citadelEnricher),
              RigSize.load[F](Path("./rigsizes.json")).map(RigSize.rigSizeEnricher)
            )
            .sequence
            .map(_.reduce[Enricher])
        _        <- staticConfig.routes.traverse { route =>
                      webhooks.activate(
                        route.webhook,
                        WebhookPayload(
                          show"""Starting up with the following filter:
                  |
                  |```lisp
                  |${route.filter.spaces2}
                  |```
                  |""".stripMargin,
                          Some(route.name)
                        )
                      )
                    }
        _        <- redisq.stream.repeat.map(enricher).through(engine).compile.drain
      yield ()
    }

  def loadConfig[F[_]: Async: Files](cli: CLI): F[StaticConfig] =
    for
      str          <- Files[F].readAll(cli.configFile).through(fs2.text.utf8.decode).compile.string
      yml          <- Async[F].fromEither(io.circe.scalayaml.parser.parse(str))
      staticConfig <- Async[F].fromEither(yml.as[StaticConfig])
    yield staticConfig

  override def run(args: List[String]): IO[ExitCode] =
    CLI.command.parse(args, sys.env) match
      case Left(value) => IO.println(value).as(ExitCode.Error)
      case Right(cli)  =>
        for
          staticConfig           <- loadConfig[IO](cli)
          given LoggerFactory[IO] = new ConsoleLoggerFactory[IO](cli.loglevel)
          _                      <- program[IO](staticConfig)
        yield ExitCode.Success
}
