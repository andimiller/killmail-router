package space.inyour.horses.killmail.router

import io.circe.Json

trait Enricher:
  def enrich(j: Json): Json
