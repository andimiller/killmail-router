package space.inyour.horses.killmail.router.redisq

import cats.effect.Concurrent
import org.http4s.client.Client
import fs2.Stream
import fs2.Chunk
import cats.implicits.*
import org.http4s.circe.*
import io.circe.Json
import org.http4s.implicits.uri

trait RedisQ[F[_]] {
  def stream: Stream[F, Json]
}

object RedisQ {

  def create[F[_]: Concurrent](client: Client[F], queueID: String) = new RedisQ[F] {
    override def stream: Stream[F, Json] =
      Stream
        .eval(
          client
            .expect[Json](uri"https://redisq.zkillboard.com/listen.php".withQueryParam("queueID", queueID))
        )
        .mapFilter { json =>
          json.hcursor.downField("package").as[Json].toOption
        }
  }

}
