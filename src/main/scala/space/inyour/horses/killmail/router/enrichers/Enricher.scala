package space.inyour.horses.killmail.router.enrichers

import cats.implicits.*
import cats.syntax.*
import cats.*
import io.circe.Json

trait Enricher extends ((Json) => Json) { outer =>
  def apply(j: Json): Json
  def liftF[F[_]: Applicative] = new EnricherF[F] {
    override def apply(j: Json): F[Json] =
      outer.apply(j).pure[F]
  }
}

object Enricher {

  given Semigroup[Enricher] = (x: Enricher, y: Enricher) => { (j: Json) =>
    y(x(j))
  }

}

trait EnricherF[F[_]] extends ((Json) => F[Json]) {
  def apply(j: Json): F[Json]
}

object EnricherF {

  given [F[_]: Parallel: Applicative]: Semigroup[EnricherF[F]] = (x: EnricherF[F], y: EnricherF[F]) => { (j: Json) =>
    (x(j), y(j)).parMapN { (xr, jr) =>
      xr.deepMerge(jr)
    }
  }

}
