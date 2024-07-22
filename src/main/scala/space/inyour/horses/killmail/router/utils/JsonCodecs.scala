package space.inyour.horses.killmail.router.utils

import cats.implicits.*
import io.circe.*
import org.http4s.Uri

object JsonCodecs {

  given Codec[Uri] = Codec.from(Decoder[String], Encoder[String]).iemap(Uri.fromString(_).leftMap(_.toString))(_.renderString)

}
