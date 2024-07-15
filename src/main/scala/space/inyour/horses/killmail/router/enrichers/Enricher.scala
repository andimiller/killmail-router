package space.inyour.horses.killmail.router.enrichers

import cats.Semigroup
import io.circe.Json

trait Enricher extends ((Json) => Json) {
  def apply(j: Json): Json
}

object Enricher {

  given Semigroup[Enricher] = (x: Enricher, y: Enricher) => { (j: Json) =>
    y(x(j))
  }

}
