package space.inyour.horses.killmail.router

import cats.{Parallel, Semigroup}
import cats.data.{NonEmptyList, Validated}
import cats.effect.*
import cats.implicits.*
import fs2.io.file.{Files, Path}
import fs2.io.net.Network
import fs2.io.net.tls.TLSContext
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.LoggerFactory
import io.circe.syntax.*
import io.circe.yaml.syntax.*
import org.typelevel.log4cats.extras.LogLevel
import space.inyour.horses.killmail.router.enrichers.{Enricher, EnricherF, Trumpets}
import space.inyour.horses.killmail.router.formatters.WebhookPayload
import space.inyour.horses.killmail.router.redisq.RedisQ
import space.inyour.horses.killmail.router.types.{Capitals, Citadels, RigSize}
import space.inyour.horses.killmail.router.maps.Systems
import space.inyour.horses.killmail.router.webhook.DiscordWebhooks
import space.inyour.horses.killmail.router.schema.Schema
import space.inyour.horses.killmail.router.template.Template
import siggy.*

import scala.concurrent.duration.*

object Main extends IOApp {

  def resources[F[_]: Parallel: Concurrent: Async: Files: Network: LoggerFactory](staticConfig: StaticConfig) =
    for
      tls           <- TLSContext.Builder.forAsync[F].insecureResource
      client        <- EmberClientBuilder
                         .default[F]
                         .withTLSContext(tls)
                         .build
      retry          =
        Retry.create[F](
          RetryPolicy(RetryPolicy.exponentialBackoff(10.minutes, 10), { case (_, resp) => RetryPolicy.isErrorOrRetriableStatus[F](resp) })
        )(client)
      webhooks       = DiscordWebhooks.create(retry)
      engine         = RulesEngine.fromStaticConfig(webhooks, staticConfig)
      siggyEnricher <- Resource.eval(
                         staticConfig.siggy
                           .traverse { siggyConfig =>
                             val siggyClient = Siggy.create[F](retry, siggyConfig.id, siggyConfig.secret)
                             siggyConfig.systems
                               .traverse { system =>
                                 SiggyEnricher.forSystem(siggyClient)(siggyConfig.chain, system, 1.minute)
                               }
                               .map(_.reduceOption(Semigroup[EnricherF[F]].combine))
                           }
                           .map(_.flatten)
                       )
      pureEnrichers <-
        Resource.eval(
          NonEmptyList
            .of(
              Systems.load[F](Path("./systems.json")).map(Systems.wormholeClassEnricher),
              Capitals.load[F](Path("./capitals.json")).map(Capitals.capitalShipEnricher),
              Citadels.load[F](Path("./citadels.json")).map(Citadels.citadelEnricher),
              RigSize.load[F](Path("./rigsizes.json")).map(RigSize.rigSizeEnricher),
              Trumpets.pure[F]
            )
            .sequence
            .map(_.reduce[Enricher])
        )
      enricher       = siggyEnricher.fold(pureEnrichers.liftF[F])(_ combine pureEnrichers.liftF[F])
    yield (retry, enricher, webhooks, engine)

  def validate[F[_]: Concurrent: Parallel: Async: Clock: Network: Files: LoggerFactory](staticConfig: StaticConfig): F[ExitCode] = {
    resources(staticConfig).use { case (_, enricher, _, _) =>
      val logger     = LoggerFactory[F].getLoggerFromName("Validator")
      val fullSchema = enricher.schema |+| Schema.zkillPayload
      for codes <- staticConfig.routes
                     .traverse { route =>
                       for
                         f <- route.filter.schema.validate(fullSchema).toEither match {
                                case Left(errors) =>
                                  errors
                                    .traverse { error =>
                                      logger.error(show"${route.name} filter - $error")
                                    }
                                    .as(ExitCode.Error)
                                case Right(_)     =>
                                  logger.info(show"${route.name} filter is valid").as(ExitCode.Success)
                              }
                         t <- route.template.getOrElse(Template.default).toSchema.validate(fullSchema).toEither match {
                                case Left(errors) =>
                                  errors
                                    .traverse { error =>
                                      logger.error(show"${route.name} template - $error")
                                    }
                                    .as(ExitCode.Error)
                                case Right(_)     =>
                                  logger.info(show"${route.name} template is valid").as(ExitCode.Success)
                              }
                       yield List(f, t).maxBy(_.code)
                     }
      yield codes.maxBy(_.code)
    }
  }

  def program[F[_]: Concurrent: Parallel: Async: Clock: Network: Files: LoggerFactory](
      staticConfig: StaticConfig,
      queueID: String,
      loglevel: LogLevel
  ): F[Unit] =
    validate[F](staticConfig) *>
      resources(staticConfig).use { case (client, enricher, webhooks, engine) =>
        val redisq = RedisQ.create(client, queueID)

        val announceStartup: F[Unit] =
          if (loglevel <= LogLevel.Debug)
            staticConfig.routes.traverse { route =>
              webhooks.activate(
                route.webhook,
                WebhookPayload(
                  show"""Starting up with the following filter:
                        |
                        |```lisp
                        |${route.filter.pretty}
                        |```
                        |""".stripMargin,
                  Some(route.name)
                )
              )
            }.void
          else ().pure[F]

        announceStartup *>
          redisq.stream.repeat.evalMap(enricher).through(engine).compile.drain
      }

  def loadConfig[F[_]: Async: Files](path: Path): F[StaticConfig] =
    for
      str          <- Files[F].readAll(path).through(fs2.text.utf8.decode).compile.string
      yml          <- Async[F].fromEither(io.circe.yaml.parser.parse(str))
      rewritten     = yml.foldWith(StaticConfig.permissiveFilterRewriter)
      staticConfig <- Async[F].fromEither(rewritten.as[StaticConfig])
    yield staticConfig

  override def run(args: List[String]): IO[ExitCode] =
    CLI.command.parse(args, sys.env) match
      case Left(value) => IO.println(value).as(ExitCode.Error)
      case Right(cli)  =>
        {
          cli match
            case CLI.Run(configFile, queueID, loglevel) =>
              for
                staticConfig           <- loadConfig[IO](configFile)
                given LoggerFactory[IO] = new ConsoleLoggerFactory[IO](loglevel)
                _                      <- program[IO](staticConfig, queueID, loglevel)
              yield ExitCode.Success
            case CLI.Format(configFile)                 =>
              for
                cfg <- loadConfig[IO](configFile)
                p    = cfg.pretty
                _   <- IO.println(p.asJson.asYaml.spaces2)
              yield ExitCode.Success
            case CLI.Validate(configFile, loglevel)     =>
              for
                cfg                    <- loadConfig[IO](configFile)
                given LoggerFactory[IO] = new ConsoleLoggerFactory[IO](loglevel)
                code                   <- validate[IO](cfg)
              yield code
        }.attemptT.leftSemiflatMap { e =>
          new ConsoleLoggerFactory[IO](LogLevel.Warn)
            .getLoggerFromClass(getClass[Main.type])
            .error(e)("Main method failed with an exception")
            .as(ExitCode.Error)
        }.merge

}
