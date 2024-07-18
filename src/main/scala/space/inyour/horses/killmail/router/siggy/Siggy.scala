package space.inyour.horses.killmail.router
package siggy

import cats.implicits.*
import cats.effect.kernel.Concurrent
import io.circe.{Codec, Json}
import io.circe.syntax.*
import org.http4s.implicits.uri
import cats.effect.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.BasicCredentials
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Headers, Request}
import org.http4s.Header
import org.http4s.client.Client
import org.http4s.Credentials.Token
import org.http4s.circe.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.Method
import org.http4s.client.Middleware
import org.http4s.implicits.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.extras.LogLevel
import space.inyour.horses.killmail.router.ConsoleLoggerFactory
import io.circe.generic.auto.*
import net.andimiller.hedgehogs.*
import net.andimiller.hedgehogs.circe.*
import scodec.bits.ByteVector
import org.http4s.client.middleware.{RequestLogger, ResponseLogger}
import enrichers.EnricherF
import scalacache.caffeine.CaffeineCache
import scalacache.{cachingF, caffeine, Entry}

import scala.concurrent.duration.*
import cats.data.NonEmptyList
import com.github.benmanes.caffeine.cache.Caffeine

import java.time.ZoneOffset
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.FiniteDuration

trait Siggy[F[_]] {
  def listChainmaps: F[Json]
  def getChainmap(id: String): F[ChainMap]
}

object Siggy {
  def create[F[_]: Concurrent: Clock](client: Client[F], id: String, secret: String) = new Siggy[F] {

    def now                                         = Clock[F].realTimeInstant.map(_.truncatedTo(ChronoUnit.SECONDS).atOffset(ZoneOffset.UTC).toString)
    def makeAuthHeader(req: Request[F]): F[Headers] = now.map { ts =>
      val sha256_HMAC = Mac.getInstance("HmacSHA256")
      val secret_key  = new SecretKeySpec(secret.getBytes, "HmacSHA256")
      sha256_HMAC.init(secret_key)
      val str         = List(req.method.toString, req.uri.path.toString.stripPrefix("/"), ts, "", "").mkString("\n")
      val hash        = ByteVector(sha256_HMAC.doFinal(str.getBytes)).toBase64
      Headers(
        Header.Raw.apply("x-siggy-date".ci, ts),
        Header.Raw.apply("Authorization".ci, s"siggy-HMAC-SHA256 Credential=$id:$hash")
      )
    }

    override def listChainmaps: F[Json] = {
      val req = Request[F](Method.GET, uri"https://siggy.horses.inyour.space/api/v1/chainmaps")
      makeAuthHeader(req).flatMap { authHeaders =>
        client.expect[Json](
          req.withHeaders(authHeaders)
        )
      }
    }

    override def getChainmap(id: String): F[ChainMap] = {
      val req = Request[F](Method.GET, uri"https://siggy.horses.inyour.space/api/v1/chainmaps/" / id)
      makeAuthHeader(req).flatMap { authHeaders =>
        client.expect[ChainMap](
          req.withHeaders(authHeaders)
        )
      }
    }
  }

}

case class Wormhole(hash: String, to_system_id: Long, from_system_id: Long) derives Codec.AsObject
case class ChainMap(id: Int, name: String, wormholes: List[Wormhole]) derives Codec.AsObject {
  def toGraph: Graph[Long, Unit, Int] = {
    val edges     = wormholes.map { wh => Edge(wh.from_system_id, wh.to_system_id, 1) }
    val bothEdges = edges ++ edges.map(e => Edge(e.to, e.from, 1))
    val nodes     = edges.map(e => Set(e.from, e.to)).reduce(_ ++ _).map(id => Node(id, ()))
    Graph.fromIterables(nodes, bothEdges, false).toOption.get // always valid
  }
}

object SiggyTest extends IOApp.Simple {
  given LoggerFactory[IO] = new ConsoleLoggerFactory[IO](LogLevel.Info)

  override def run: IO[Unit] =
    EmberClientBuilder.default[IO].build.use { client =>
      val wrappedClient = ResponseLogger(true, true)(
        RequestLogger(true, true)(client)
      )
      val siggy         = Siggy.create[IO](wrappedClient, "REDACTED", "REDACTED")

      for
        r <- siggy.listChainmaps
        _ <- IO.println(r)
      yield ()
    }
}

object SiggyEnricher {

  def forSystem[F[_]: Sync](siggy: Siggy[F])(chain: String, system: Long, ttl: FiniteDuration): F[EnricherF[F]] =
    Sync[F].delay(Caffeine.newBuilder.build[Long, Entry[Map[Long, Int]]]()).map(CaffeineCache(_)).map { implicit cache =>
      new EnricherF[F] {
        override def apply(j: Json): F[Json] =
          cachingF[F, Long, Map[Long, Int]](system)(Some(ttl)) {
            siggy.getChainmap(chain).flatMap { cm =>
              Sync[F].delay {
                val graph = cm.toGraph
                Dijkstra.multi(graph)(system, graph.nodes.keySet - system).view.mapValues(_._1).toMap
              }
            }
          }.map { distances =>
            j.hcursor.downField("killmail").downField("solar_system_id").as[Long] match
              case Left(_)              => j
              case Right(solarSystemId) =>
                j.deepMerge(
                  Json.obj(
                    "chain_distance" :=
                      Json.obj(
                        system.toString := distances.get(solarSystemId)
                      )
                  )
                )
          }
      }
    }

}
