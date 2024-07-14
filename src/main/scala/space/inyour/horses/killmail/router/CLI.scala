package space.inyour.horses.killmail.router

import cats.implicits.*
import com.monovore.decline.{Argument, Command, Opts}
import fs2.io.file.Path
import org.typelevel.log4cats.extras.LogLevel

case class CLI(
    configFile: Path,
    loglevel: LogLevel
)
object CLI {
  given Argument[Path]      = Argument[String].map(Path(_))
  given Argument[LogLevel]  = Argument.from("level") { s =>
    LogLevel.fromString(s).toValidNel("Please select a valid log level")
  }
  val command: Command[CLI] =
    Command("router", "killmail router", true)(
      (
        Opts.argument[Path]("config.yml"),
        Opts.env[LogLevel]("LOGLEVEL", "level to run the logger at").withDefault(LogLevel.Info)
      ).mapN(CLI.apply)
    )

}
