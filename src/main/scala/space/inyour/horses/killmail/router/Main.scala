package space.inyour.horses.killmail.router

import cats.effect.*
import cats.implicits.*

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO.pure(ExitCode.Success)
}
