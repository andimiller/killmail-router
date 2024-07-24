package space.inyour.horses.killmail.router

import filters.{Expr, PathOperation}
import net.andimiller.cats.parse.interpolator.*
import cats.implicits.*
import cats.parse.Parser
import io.circe.*
import io.circe.syntax.*
import cats.Show
import space.inyour.horses.killmail.router.schema.Schema
import space.inyour.horses.killmail.router.filters.Expr

package object template {

  lazy val printJson: Json.Folder[String] = new Json.Folder[String] {
    override def onArray(value: Vector[Json]): String = value.asJson.toString
    override def onObject(value: JsonObject): String  = value.toJson.toString
    override def onString(value: String): String      = value
    override def onNumber(value: JsonNumber): String  = value.toString
    override def onBoolean(value: Boolean): String    = value.toString
    override def onNull: String                       = "Unknown"
  }

  case class Template(exprs: Vector[TemplateExpr]):
    def render(payload: Json): String =
      exprs.iterator.map {
        case TemplateExpr.Literal(s)     => s
        case TemplateExpr.Variable(path) => Expr.evaluatePath(path)(payload).getOrElse(Json.Null).foldWith(printJson)
      }.mkString
    def simplify: Template            = Template(
      fs2.Stream
        .emits(exprs)
        .groupAdjacentBy {
          case TemplateExpr.Literal(_)  => true
          case TemplateExpr.Variable(_) => false
        }
        .map {
          case (true, group)  =>
            fs2.Chunk(
              TemplateExpr.Literal(group.iterator.collect { case TemplateExpr.Literal(s) => s }.mkString)
            )
          case (false, group) => group
        }
        .unchunks
        .compile
        .toVector
    )
    def toSchema: Schema              = exprs
      .collect { case TemplateExpr.Variable(path) =>
        Expr.pathSchema(path, Schema.SAny)
      }
      .reduceOption(_ |+| _)
      .getOrElse(Schema.SObject(Map()))

  enum TemplateExpr:
    case Literal(s: String)
    case Variable(path: List[PathOperation])

  object Template:
    import TemplateExpr.*
    val default: Template   = Template(
      Vector(
        Literal("https://zkillboard.com/kill/"),
        Variable(List(PathOperation.DownField("killID")))
      )
    )
    given Decoder[Template] = Decoder[String].emap { s =>
      parser.parseAll(s).leftMap(_.show)
    }
    given Encoder[Template] = Encoder[String].contramap(_.show)

    import Expr.showListPathOperation
    given Show[TemplateExpr] = Show.show {
      case TemplateExpr.Literal(s)     => s
      case TemplateExpr.Variable(path) => show"$${$path}"
    }
    given Show[Template]     = Show.show(_.exprs.mkString_(""))

    val parser: Parser[Template] =
      Parser
        .oneOf(
          List(
            p"$${${Expr.pathParser}}".map(TemplateExpr.Variable.apply),
            Parser.charsWhile(_ != '$').map(TemplateExpr.Literal.apply)
          )
        )
        .rep
        .map(_.iterator.toVector)
        .map(Template.apply)

}
