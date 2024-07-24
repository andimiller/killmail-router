package space.inyour.horses.killmail.router.enrichers

import io.circe.Json
import io.circe.syntax.*

object Trumpets extends Enricher {
  val trumpet: String               = "\uD83C\uDFBA"
  val trumpet_value: Double         = 500_000_000.0d
  override def apply(j: Json): Json = {
    val lossValue = j.hcursor.downField("zkb").downField("totalValue").focus.flatMap(_.asNumber).map(_.toDouble).getOrElse(0.0d)

    val trumpetCount = (lossValue / trumpet_value).toInt
    val trumpets     = trumpet * trumpetCount
    j.withObject(_.add("trumpets", Json.fromString(trumpets)).asJson)
  }
}
