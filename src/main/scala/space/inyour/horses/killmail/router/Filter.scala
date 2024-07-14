package space.inyour.horses.killmail.router

import io.circe.Json

trait Filter:
  def check(j: Json): Boolean
