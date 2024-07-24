package space.inyour.horses.killmail.router

import cats.implicits.*
import com.monovore.decline.{Argument, Command, Opts}
import fs2.io.file.Path
import org.typelevel.log4cats.extras.LogLevel

enum CLI:
  case Run(
      configFile: Path,
      queueID: String,
      loglevel: LogLevel
  )
  case Format(
      configFile: Path
  )
  case Validate(
      configFile: Path,
      loglevel: LogLevel
  )

object CLI {
  given Argument[Path]     = Argument[String].map(Path(_))
  given Argument[LogLevel] = Argument.from("level") { s =>
    LogLevel.fromString(s).toValidNel("Please select a valid log level")
  }

  val run: Command[CLI.Run] = Command("run", "run the killmail router", true)(
    (
      Opts.argument[Path]("config.yml"),
      Opts.argument[String]("queueID"),
      Opts.env[LogLevel]("LOGLEVEL", "level to run the logger at").withDefault(LogLevel.Info)
    ).mapN(CLI.Run.apply)
  )

  val format: Command[CLI.Format] = Command("fmt", "reformat the config file", true)(
    Opts.argument[Path]("config.yml").map(CLI.Format.apply)
  )

  val validate: Command[CLI.Validate] = Command("validate", "validate the config file", true)(
    (
      Opts.argument[Path]("config.yml"),
      Opts.env[LogLevel]("LOGLEVEL", "level to run the logger at").withDefault(LogLevel.Info)
    ).mapN(CLI.Validate.apply)
  )

  val command: Command[CLI] =
    Command("router", "killmail router", true)(
      Opts.subcommand(run).orElse(Opts.subcommand(format)).orElse(Opts.subcommand(validate))
    )

}
