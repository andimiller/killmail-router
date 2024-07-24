package space.inyour.horses.killmail.router.enrichers

import cats.implicits.*
import cats.syntax.*
import cats.*
import io.circe.Json
import space.inyour.horses.killmail.router.schema.Schema

trait Enricher extends ((Json) => Json) { outer =>
  def apply(j: Json): Json
  def liftF[F[_]: Applicative] = new EnricherF[F] {
    override def apply(j: Json): F[Json] =
      outer.apply(j).pure[F]
    override def schema: Schema          = outer.schema
  }
  def schema: Schema
}

object Enricher {

  given Semigroup[Enricher] = (x: Enricher, y: Enricher) =>
    new Enricher {
      override def apply(j: Json): Json = y(x(j))
      override def schema: Schema       = x.schema |+| y.schema
    }

}

trait EnricherF[F[_]] extends ((Json) => F[Json]) {
  def apply(j: Json): F[Json]
  def schema: Schema
}

object EnricherF {

  given [F[_]: Parallel: Applicative]: Semigroup[EnricherF[F]] = (x: EnricherF[F], y: EnricherF[F]) =>
    new EnricherF[F] {
      override def apply(j: Json): F[Json] = (x(j), y(j)).parMapN { (xr, jr) =>
        xr.deepMerge(jr)
      }
      override def schema: Schema          = x.schema |+| y.schema
    }

}
