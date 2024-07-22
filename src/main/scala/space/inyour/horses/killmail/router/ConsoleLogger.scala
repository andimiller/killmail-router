package space.inyour.horses.killmail.router

import cats.implicits.*
import cats.Monad
import cats.effect.kernel.Clock
import cats.effect.std.Console
import org.typelevel.log4cats.*
import org.typelevel.log4cats.extras.LogLevel
import org.typelevel.log4cats.extras.LogLevel.*

import java.io.{ByteArrayOutputStream, PrintStream}
import java.time.ZoneOffset

class ConsoleLoggerFactory[F[_]: Monad: Console: Clock](logLevel: LogLevel) extends LoggerFactory[F] {

  override def getLoggerFromName(name: String): SelfAwareStructuredLogger[F] = new ConsoleLogger[F](name, logLevel)

  override def fromName(name: String): F[SelfAwareStructuredLogger[F]] = new ConsoleLogger[F](name, logLevel).pure[F]
}

class ConsoleLogger[F[_]: Monad: Console: Clock](name: String, var logLevel: LogLevel) extends SelfAwareStructuredLogger[F] {

  def log(level: LogLevel, msg: String, throwable: Option[Throwable]): F[Unit] = if (level >= logLevel) {
    for {
      ts <- Clock[F].realTimeInstant.map(_.atZone(ZoneOffset.UTC))
      _  <- Console[F].println(s"$ts - $level - $name - $msg")
      _  <- throwable
              .map { t =>
                val baos = new ByteArrayOutputStream()
                val ps   = new PrintStream(baos)
                t.printStackTrace(ps)
                baos.toString
              }
              .traverse(Console[F].println(_))
    } yield ()
  } else ().pure[F]

  override def trace(t: Throwable)(message: => String): F[Unit] = log(LogLevel.Trace, message, Some(t))
  override def trace(message: => String): F[Unit]               = log(LogLevel.Trace, message, None)
  override def isTraceEnabled: F[Boolean]                       = (logLevel <= Trace).pure[F]

  override def debug(t: Throwable)(message: => String): F[Unit] = log(LogLevel.Debug, message, Some(t))
  override def debug(message: => String): F[Unit]               = log(LogLevel.Debug, message, None)
  override def isDebugEnabled: F[Boolean]                       = (logLevel <= Debug).pure[F]

  override def info(t: Throwable)(message: => String): F[Unit] = log(LogLevel.Info, message, Some(t))
  override def info(message: => String): F[Unit]               = log(LogLevel.Info, message, None)
  override def isInfoEnabled: F[Boolean]                       = (logLevel <= Info).pure[F]

  override def warn(t: Throwable)(message: => String): F[Unit] = log(LogLevel.Warn, message, Some(t))
  override def warn(message: => String): F[Unit]               = log(LogLevel.Warn, message, None)
  override def isWarnEnabled: F[Boolean]                       = (logLevel <= Warn).pure[F]

  override def error(t: Throwable)(message: => String): F[Unit] = log(LogLevel.Error, message, Some(t))
  override def error(message: => String): F[Unit]               = log(LogLevel.Error, message, None)
  override def isErrorEnabled: F[Boolean]                       = (logLevel <= Error).pure[F]

  /*
   * ConsoleLogger should probably not extend from StructuredLogger, because there's not
   * a good way to use the context map on this platform. However, LoggerFactory forces
   * its LoggerType to extend SelfAwareStructuredLogger, and since that's the factory
   * type that is well documented, that's what is demanded everywhere. Therefore, to be
   * useful, we implement the context variants below, but completely ignore the context
   * map parameters.
   */
  override def trace(ctx: Map[String, String])(msg: => String): F[Unit]               = trace(msg)
  override def trace(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    trace(t)(msg)
  override def debug(ctx: Map[String, String])(msg: => String): F[Unit]               = debug(msg)
  override def debug(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    debug(t)(msg)
  override def info(ctx: Map[String, String])(msg: => String): F[Unit]                = info(msg)
  override def info(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit]  = info(t)(msg)
  override def warn(ctx: Map[String, String])(msg: => String): F[Unit]                = warn(msg)
  override def warn(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit]  = warn(t)(msg)
  override def error(ctx: Map[String, String])(msg: => String): F[Unit]               = error(msg)
  override def error(ctx: Map[String, String], t: Throwable)(msg: => String): F[Unit] =
    error(t)(msg)
}
